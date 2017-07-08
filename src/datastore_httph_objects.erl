%% ----------------------------------------------------------------------------
%% The MIT License
%%
%% Copyright (c) 2016-2017 Andrei Nesterov <ae.nesterov@gmail.com>
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

-module(datastore_httph_objects).

-include_lib("riaks2c/include/riaks2c_xsd.hrl").
-include("datastore_log.hrl").

%% REST handler callbacks
-export([
	init/2,
	malformed_request/2,
	is_authorized/2,
	forbidden/2,
	content_types_provided/2,
	allowed_methods/2,
	options/2
]).

%% Content callbacks
-export([
	to_json/2
]).

%% Types
-record(state, {
	rdesc        :: map(),
	authconf     :: map(),
	bucket       :: iodata(),
	params = #{} :: map(),
	authm  = #{} :: map()
}).

%% =============================================================================
%% REST handler callbacks
%% =============================================================================

init(Req, Opts) ->
	#{authentication := AuthConf, resources := Rdesc} = Opts,
	State =
		#state{
			rdesc = Rdesc,
			authconf = AuthConf,
			bucket = cowboy_req:binding(bucket, Req)},
	{cowboy_rest, Req, State}.

malformed_request(Req, State) ->
	CtL =
		[	{lexmarker, datastore_constraint:binary(1, 1024), undefined},
			{lexprefix, datastore_constraint:binary(1, 1024), undefined},
			{rows, datastore_constraint:int(0, 100), 100} ],
	try cowboy_req:match_qs(CtL, Req) of
		Qs  -> {false, Req, State#state{params = Qs}}
	catch
		T:R ->
			?ERROR_REPORT(datastore_http_log:format_request(Req), T, R),
			{true, Req, State}
	end.

is_authorized(#{method := <<"OPTIONS">>} =Req, State)  -> {true, Req, State};
is_authorized(Req, #state{authconf = AuthConf} =State) ->
	try datastore_http:decode_access_token(Req, AuthConf) of
		TokenPayload ->
			?INFO_REPORT([{access_token, TokenPayload} | datastore_http_log:format_request(Req)]),
			{true, Req, State#state{authm = TokenPayload}}
	catch
		T:R ->
			?ERROR_REPORT(datastore_http_log:format_unauthenticated_request(Req), T, R),
			{{false, datastore_http:access_token_type()}, Req, State}
	end.

forbidden(#{method := <<"OPTIONS">>} =Req, State)                            -> {false, Req, State};
forbidden(Req, #state{rdesc = Rdesc, bucket = Bucket, authm = AuthM} =State) ->
	try datastore:authorize(Bucket, AuthM, Rdesc) of
		{ok, #{read := true}} -> {false, Req, State};
		_                     -> {true, Req, State}
	catch T:R ->
		?ERROR_REPORT(datastore_http_log:format_request(Req), T, R),
		{stop, cowboy_req:reply(422, Req), State}
	end.

content_types_provided(Req, State) ->
	Handlers = [{{<<"application">>, <<"json">>, '*'}, to_json}],
	{Handlers, Req, State}.

allowed_methods(Req, State) ->
	Methods = [<<"GET">>, <<"OPTIONS">>],
	{Methods, Req, State}.

options(Req0, State) ->
	Req1 = cowboy_req:set_resp_header(<<"access-control-allow-methods">>, <<"GET, POST">>, Req0),
	Req2 = cowboy_req:set_resp_header(<<"access-control-allow-headers">>, <<"authorization">>, Req1),
	Req3 = cowboy_req:set_resp_header(<<"access-control-allow-credentials">>, <<"true">>, Req2),
	{ok, Req3, State}.

%% =============================================================================
%% Content callbacks
%% =============================================================================

to_json(Req, #state{bucket = Bucket, params = Params, rdesc = Rdesc} =State) ->
	datastore_http:handle_response(Req, State, fun() ->
		jsx:encode(handle_list(Bucket, riaks2_reqopts(Params), Rdesc))
	end).

%% =============================================================================
%% Internal functions
%% =============================================================================

-spec handle_list(binary(), riaks2c_http:request_options(), map()) -> [map()].
handle_list(Bucket, ReqOpts, Rdesc) ->
	#{object := #{pool := S2pool, options := S2opts}} = Rdesc,
	S2pid = gunc_pool:lock(S2pool),
	Resp = riaks2c_object:await_list(S2pid, riaks2c_object:list(S2pid, Bucket, ReqOpts, S2opts)),
	gunc_pool:unlock(S2pool, S2pid),

	error_logger:info_report([Resp]),
	case Resp of
		{error, {bad_bucket, _Bucket}}                    -> [];
		{ok, #'ListBucketResult'{'Contents' = undefined}} -> [];
		{ok, #'ListBucketResult'{'Contents' = L}} ->
			[ #{id => Key, data => #{size => binary_to_integer(Size)}}
				|| #'ListEntry'{'Key' = Key, 'Size' = Size} <- L ]
	end.

-spec riaks2_reqopts(map()) -> riaks2c_http:request_options().
riaks2_reqopts(Params) ->
	Qs =
		maps:fold(
			fun
				(_Key, undefined, Acc) -> Acc;
				(Key, Val, Acc)        -> [riaks2_reqopt(Key, Val)|Acc]
			end, [], Params),
	#{qs => Qs}.

-spec riaks2_reqopt(atom(), any()) -> {binary(), any()}.
riaks2_reqopt(lexmarker, Val) -> {<<"marker">>, Val};
riaks2_reqopt(lexprefix, Val) -> {<<"prefix">>, Val};
riaks2_reqopt(rows, Val)      -> {<<"max-keys">>, integer_to_binary(Val)}.
