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
	authconf        :: map(),
	r               :: map(),
	key             :: iodata(),
	bucket          :: iodata(),
	params    = #{} :: map(),
	s2reqopts = #{} :: riaks2c_http:request_options(),
	token_payload   :: map() | undefined
}).

%% =============================================================================
%% HTTP handler callbacks
%% =============================================================================

init(Req, Opts) ->
	#{method := Method} = Req,
	#{authentication := AuthConf, resources := R} = Opts,
	State =
		#state{
			authconf = AuthConf,
			r = R,
			key = cowboy_req:binding(key, Req),
			bucket = cowboy_req:binding(bucket, Req)},

	handle_request(Method, Req, State).

%% =============================================================================
%% Internal functions
%% =============================================================================

handle_request(<<"GET">>, Req, State) ->
	handle_headers(allow_read_headers(), Req, State);
handle_request(<<"OPTIONS">>, Req0, State) ->
	Req1 = cowboy_req:set_resp_header(<<"access-control-allow-methods">>, <<"GET">>, Req0),
	Req2 = cowboy_req:set_resp_header(<<"access-control-allow-headers">>, <<"Authorization, Cache-Control, Range">>, Req1),
	{ok, cowboy_req:reply(200, Req2), State};
handle_request(_, Req, State) ->
	{stop, cowboy_req:reply(405, Req), State}.

handle_headers(AllowedHs, #{headers := Hs} =Req, State) ->
	try with_headers(AllowedHs, Hs) of
		S2headers ->
			handle_params(Req, State#state{s2reqopts = #{headers => S2headers}})
	catch
		T:R ->
			?ERROR_REPORT([{http_headers, Hs} | datastore_http_log:format_request(Req)], T, R),
			{stop, cowboy_req:reply(400, Req), State}
	end.

handle_params(#{method := Method} =Req, State) ->
	try parse_params(Method, cowboy_req:parse_qs(Req)) of
		Params ->
			handle_authentication(Req, State#state{params = Params})
	catch
		T:R ->
			?ERROR_REPORT(datastore_http_log:format_request(Req), T, R),
			{stop, cowboy_req:reply(400, Req), State}
	end.

handle_authentication(Req, #state{authconf = AuthConf, params = Params} =State) ->
	try case Params of
		#{access_token := Token} -> datastore:decode_access_token(Token, AuthConf);
		_                        -> datastore_http:decode_access_token(Req, AuthConf)
	end of TokenPayload ->
		?INFO_REPORT([{access_token, TokenPayload} | datastore_http_log:format_request(Req)]),
		handle_read(Req, State#state{token_payload = TokenPayload})
	catch T:R ->
		?ERROR_REPORT(datastore_http_log:format_unauthenticated_request(Req), T, R),
		{stop, cowboy_req:reply(401, Req), State}
	end.

handle_read(Req0, #state{r = Resources, key = Key, params = Params, bucket = Bucket, s2reqopts = S2reqopts} =State) ->
	#{object := #{pool := Pool, options := S2opts, handler := Hmod}} = Resources,

	Pid = gunc_pool:lock(Pool),
	Ref = riaks2c_object:get(Pid, Bucket, Key, S2reqopts, S2opts),
	Req1 =
		try handle_read_stream(Hmod, Bucket, Key, Params, Pid, Ref, ?REQUEST_TIMEOUT, Req0)
		catch T:R ->
			?ERROR_REPORT(datastore_http_log:format_request(Req0), T, R),
			cowboy_req:reply(422, Req0)
		end,
	gunc_pool:unlock(Pool, Pid),
	{ok, Req1, State}.

-spec handle_read_stream(module(), binary(), binary(), map(), pid(), reference(), non_neg_integer(), Req) -> Req when Req :: cowboy_req:req().
handle_read_stream(Hmod, Bucket, Key, Params, Pid, Ref, Timeout, Req) ->
	Mref = monitor(process, Pid),
	case riaks2c_object:expect_head(Pid, Ref, Timeout, Mref) of
		{Status, Headers} when Status >= 200, Status < 300 ->
			case Hmod:handle_read(Bucket, Key, Params, Status, Headers) of
				{stream, Hst, Hhs, Hstate} ->
					Stream = cowboy_req:stream_reply(Hst, Hhs, Req),
					riaks2c_http:fold_body(
						Pid, Ref, Timeout, Mref, Hstate,
						fun(Data, IsFin, Acc) ->
							Hmod:handle_read_stream(Data, IsFin, Stream, Acc)
						end),
					Stream;
				{await_body, Hstate} ->
					Body = riaks2c_object:expect_body(Pid, Ref, Timeout, Mref),
					case Hmod:handle_read_body(Status, Headers, Body, Hstate) of
						{Hst, Hhs, keep_body} -> cowboy_req:reply(Hst, Hhs, Body, Req);
						{Hst, Hhs, Hbody}     -> cowboy_req:reply(Hst, Hhs, Hbody, Req)
					end
			end;
		{404, _Headers} ->
			cowboy_req:reply(404, Req);
		{Status, _Headers} ->
			exit({bad_riaks2_response_status, Status})
	end.

-spec allow_read_headers() -> [Validator] when Validator :: {binary(), fun((iodata()) -> any())}.
allow_read_headers() ->
	[	{<<"range">>, fun cow_http_hd:parse_range/1},
		{<<"cache-control">>, fun cow_http_hd:parse_cache_control/1} ].

-spec parse_params(binary(), [{binary(), binary() | true}]) -> map().
parse_params(<<"GET">>, Qs) ->
	parse_read_params(Qs, #{}).

-spec parse_read_params([{binary(), binary() | true}], map()) -> map().
parse_read_params([{<<"access_token">>, Val}|T], M) -> parse_read_params(T, M#{access_token => Val});
parse_read_params([_|T], M)                         -> parse_read_params(T, M);
parse_read_params([], M)                            -> M.

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
