%% ----------------------------------------------------------------------------
%% The MIT License
%%
%% Copyright (c) 2016 Andrei Nesterov <ae.nesterov@gmail.com>
%%
%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to
%% deal in the Software without restriction, including without limitation the
%% rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
%% sell copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
%% FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
%% IN THE SOFTWARE.
%% ----------------------------------------------------------------------------

-module(datastore_httph_object).

-include("datastore_log.hrl").

%% HTTP handler callbacks
-export([
	init/2
]).

%% Definitions
-define(REQUEST_TIMEOUT, 5000).

%% Types
-record(state, {
	authconf  :: map(),
	r         :: map(),
	key       :: iodata(),
	bucket    :: iodata(),
	s2reqopts :: riaks2c_http:request_options() | undefined
}).

%% =============================================================================
%% HTTP handler callbacks
%% =============================================================================

init(Req, Opts) ->
	#{method := Method} = Req,
	#{authentication := Auth, resources := R} = Opts,
	State =
		#state{
			authconf = Auth,
			r = R,
			key = cowboy_req:binding(key, Req),
			bucket = cowboy_req:binding(bucket, Req)},

	handle_request(Method, Req, State).

%% =============================================================================
%% Internal function
%% =============================================================================

handle_request(<<"GET">>, #{headers := Hs} =Req, State) ->
	try request_headers(Hs) of
		S2headers ->
			handle_authentication(Req, State#state{s2reqopts = #{headers => S2headers}})
	catch
		_:R ->
			?WARNING_REPORT([{bad_request, R} | datastore_http_log:format_request(Req)]),
			{stop, cowboy_req:reply(400, Req), State}
	end;
handle_request(<<"OPTIONS">>, Req0, State) ->
	Req1 = cowboy_req:set_resp_header(<<"access-control-allow-methods">>, <<"GET">>, Req0),
	Req2 = cowboy_req:set_resp_header(<<"access-control-allow-headers">>, <<"Authorization, Cache-Control, Range">>, Req1),
	{ok, Req2, State};
handle_request(_, Req, State) ->
	{stop, cowboy_req:reply(405, Req), State}.

handle_authentication(Req, #state{authconf = Auth} =State) ->
	try
		TokenPayload = datastore_http:decode_access_token(Req, Auth),
		?INFO_REPORT([{access_token, TokenPayload} | datastore_http_log:format_request(Req)]),
		handle_read(Req, State)
	catch T:R ->
		?ERROR_REPORT(datastore_http_log:format_unauthenticated_request(Req), T, R),
		{stop, cowboy_req:reply(401, Req), State}
	end.

handle_read(Req0, #state{r = Resources, key = Key, bucket = Bucket, s2reqopts = S2reqopts} =State) ->
	#{object := #{pool := Pool, options := S2opts}} = Resources,

	Pid = gunc_pool:lock(Pool),
	Ref = riaks2c_object:get(Pid, Bucket, Key, S2reqopts, S2opts),
	Req1 = handle_read_stream(Pid, Ref, ?REQUEST_TIMEOUT, Req0),
	gunc_pool:unlock(Pool, Pid),
	{ok, Req1, State}.

-spec handle_read_stream(pid(), reference(), non_neg_integer(), Req) -> Req when Req :: cowboy_req:req().
handle_read_stream(Pid, Ref, Timeout, Req) ->
	Mref = monitor(process, Pid),
	case gun:await(Pid, Ref, Mref) of
		{response, nofin, Status, Headers} when Status =:= 200; Status =:= 206 ->
			Stream = cowboy_req:stream_reply(Status, set_response_headers(Headers, Req)),
			riaks2c_http:fold_body(
				Pid, Ref, Timeout, Mref, ignore,
				fun(Data, IsFin, _) ->
					cowboy_req:stream_body(Data, IsFin, Stream)
				end),
			Stream;
		{response, _IsFin, 404, _Headers} ->
			cowboy_req:reply(404, Req);
		{response, _IsFin, _Status, _Headers} ->
			?ERROR_REPORT([{bad_http_status, _Status} | datastore_http_log:format_request(Req)]),
			cowboy_req:reply(422, Req)
	end.

-spec request_headers(map()) -> [{binary(), iodata()}].
request_headers(Headers) ->
	with_headers(
		[	{<<"range">>, fun cow_http_hd:parse_range/1},
			{<<"cache-control">>, fun cow_http_hd:parse_cache_control/1} ],
		Headers).

-spec with_headers([Validator], map()) -> Headers
	when
		Validator :: {binary(), fun((iodata()) -> any())},
		Headers :: [{binary(), iodata()}].
with_headers(Keys, Headers) ->
	with_headers(Keys, Headers, []).

-spec with_headers([Validator], map(), Headers) -> Headers
	when
		Validator :: {binary(), fun((iodata()) -> any())},
		Headers :: [{binary(), iodata()}].
with_headers([{Key, Validate}|T], Headers, Acc) ->
	case maps:find(Key, Headers) of
		{ok, Val} ->
			try Validate(Val) of
				_ -> with_headers(T, Headers, [{Key, Val}|Acc])
			catch _:_ -> error({bad_range, Val}) end;
		_ ->
			with_headers(T, Headers, Acc)
	end;
with_headers([], _Headers, Acc) ->
	Acc.

-spec set_response_headers(Headers, Req) -> Req
	when
		Headers :: [{binary(), iodata()}],
		Req :: cowboy_req:req().
set_response_headers([{<<"server">>, _}|T], Req)     -> set_response_headers(T, Req);
set_response_headers([{<<"x-", _/bits>>, _}|T], Req) -> set_response_headers(T, Req);
set_response_headers([{Key,Val}|T], Req)             -> set_response_headers(T, cowboy_req:set_resp_header(Key, Val, Req));
set_response_headers([], Req)                        -> Req.
