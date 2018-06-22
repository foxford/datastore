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

-module(datastore_httph_sign).

-include("datastore_log.hrl").

%% REST handler callbacks
-export([
	init/2,
	is_authorized/2,
	% forbidden/2,
	% content_types_provided/2,
	content_types_accepted/2,
	allowed_methods/2,
	options/2
]).

%% Content callbacks
-export([
	from_json/2
]).

%% Types
-record(state, {
	rdesc              :: map(),
	authconf           :: map(),
	authm  = #{}       :: map()
}).

%% =============================================================================
%% REST handler callbacks
%% =============================================================================

init(Req, Opts) ->
	#{authentication := AuthConf, resources := Rdesc} = Opts,
	State =
		#state{
			rdesc = Rdesc,
			authconf = AuthConf },
	{cowboy_rest, Req, State}.

is_authorized(#{method := <<"OPTIONS">>} =Req, State)  -> {true, Req, State};
is_authorized(Req, #state{authconf = AuthConf} =State) ->
	try datastore_http:decode_access_token(Req, AuthConf) of
		TokenPayload ->
			%% ?INFO_REPORT([{access_token, TokenPayload} | datastore_http_log:format_request(Req)]),
			{true, Req, State#state{authm = TokenPayload}}
	catch
		T:R ->
			?ERROR_REPORT(datastore_http_log:format_unauthenticated_request(Req), T, R),
			{{false, datastore_http:access_token_type()}, Req, State}
	end.

% forbidden(#{method := <<"OPTIONS">>} =Req, State)                            -> {false, Req, State};
% forbidden(Req, #state{rdesc = Rdesc, bucket = Bucket, authm = AuthM} =State) ->
% 	try datastore:authorize(Bucket, AuthM, Rdesc) of
% 		{ok, #{write := true}} -> {false, Req, State};
% 		_                      -> {true, Req, State}
% 	catch T:R ->
% 		?ERROR_REPORT(datastore_http_log:format_request(Req), T, R),
% 		{stop, cowboy_req:reply(422, Req), State}
% 	end.

% content_types_provided(Req, State) ->
% 	Handlers = [{{<<"application">>, <<"json">>, '*'}, to_json}],
% 	{Handlers, Req, State}.

content_types_accepted(Req, State) ->
	Handlers = [{{<<"application">>, <<"json">>, '*'}, from_json}],
	{Handlers, Req, State}.

allowed_methods(Req, State) ->
	Methods = [<<"POST">>, <<"OPTIONS">>],
	{Methods, Req, State}.

options(Req0, State) ->
	Req1 = cowboy_req:set_resp_header(<<"access-control-allow-methods">>, <<"POST">>, Req0),
	Req2 = cowboy_req:set_resp_header(<<"access-control-allow-headers">>, <<"authorization, content-length, content-type">>, Req1),
	Req3 = cowboy_req:set_resp_header(<<"access-control-allow-credentials">>, <<"true">>, Req2),
	{ok, Req3, State}.

%% =============================================================================
%% Content callbacks
%% =============================================================================

from_json(Req0, #state{authm = AuthM, rdesc = Rdesc} =State) ->
	#{object := #{options := S2opts, redirect := #{host := Host, port := Port, schema := Schema}}} = Rdesc,
	datastore_http:handle_payload(Req0, State, fun(Payload, _Req1) ->
		Params = verify_resource(jsx:decode(Payload)),

		%% Payload
		#{	bucket := Bucket,
			key := Key,
			method := Method,
			headers := Headers } = Params,
		S2reqopts = #{headers => Headers},
		Set = maps:get(set, Params, undefined),
		Expires = datastore:unix_time() + datastore:expires_in(),

		%% AuthZ
		{ok, #{write := true}} = datastore:authorize(Bucket, AuthM, Rdesc),

		datastore_http:handle_response(_Req1, State, fun() ->
			Path = riaks2c_object:signed_uri(Bucket, datastore:object_key(Set, Key), Method, Expires, S2reqopts, S2opts),
			Location = iolist_to_binary([Schema, Host, <<$:>>, integer_to_binary(Port), Path]),
			#{uri => Location}
		end)
	end).

-spec verify_resource([{binary(), any()}]) -> map().
verify_resource(Val) ->
    verify_resource(Val, #{}).

-spec verify_resource([{binary(), any()}], map()) -> map().
verify_resource([{<<"bucket">>, Val}|T], Acc)    -> verify_resource(T, Acc#{bucket => verify_resource_bucket(Val)});
verify_resource([{<<"set">>, Val}|T], Acc)       -> verify_resource(T, Acc#{set => verify_resource_set(Val)});
verify_resource([{<<"key">>, Val}|T], Acc)       -> verify_resource(T, Acc#{key => verify_resource_key(Val)});
verify_resource([{<<"method">>, Val}|T], Acc)    -> verify_resource(T, Acc#{method => verify_resource_method(Val)});
verify_resource([{<<"headers">>, Val}|T], Acc)   -> verify_resource(T, Acc#{headers => verify_resource_headers(Val)});
verify_resource(_, Acc)                          -> Acc.

-spec verify_resource_bucket(binary()) -> binary().
verify_resource_bucket(Val) when is_binary(Val) -> Val;
verify_resource_bucket(Val)                     -> error({bad_bucket, Val}).

-spec verify_resource_set(binary()) -> binary().
verify_resource_set(Val) when is_binary(Val) -> Val;
verify_resource_set(Val)                     -> error({bad_set, Val}).

-spec verify_resource_key(binary()) -> binary().
verify_resource_key(Val) when is_binary(Val) -> Val;
verify_resource_key(Val)                     -> error({bad_key, Val}).

-spec verify_resource_method(binary()) -> binary().
verify_resource_method(Val) when is_binary(Val) -> Val;
verify_resource_method(Val)                     -> error({bad_method, Val}).

-spec verify_resource_headers([{binary(), any()}]) -> [{binary(), any()}].
verify_resource_headers(Val) ->
	try
		[ok || {K, _V} <- Val, true = is_binary(K)],
		Val
	catch _:_ -> error({bad_headers, Val}) end.