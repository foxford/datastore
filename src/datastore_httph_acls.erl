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

-module(datastore_httph_acls).

-include("datastore_log.hrl").

%% REST handler callbacks
-export([
	init/2,
	is_authorized/2,
	forbidden/2,
	content_types_provided/2,
	content_types_accepted/2,
	allowed_methods/2,
	options/2
]).

%% Content callbacks
-export([
	from_json/2,
	to_json/2
]).

%% Types
-record(state, {
	rdesc              :: map(),
	authconf           :: map(),
	bucket             :: iodata(),
	key    = undefined :: iodata(),
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
			authconf = AuthConf,
			bucket = cowboy_req:binding(bucket, Req),
			key = cowboy_req:binding(key, Req)},
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

forbidden(#{method := <<"OPTIONS">>} =Req, State)                            -> {false, Req, State};
forbidden(Req, #state{rdesc = Rdesc, bucket = Bucket, authm = AuthM} =State) ->
	try datastore:authorize(Bucket, AuthM, Rdesc) of
		{ok, #{write := true}} -> {false, Req, State};
		_                      -> {true, Req, State}
	catch T:R ->
		?ERROR_REPORT(datastore_http_log:format_request(Req), T, R),
		{stop, cowboy_req:reply(422, Req), State}
	end.

content_types_provided(Req, State) ->
	Handlers = [{{<<"application">>, <<"json">>, '*'}, to_json}],
	{Handlers, Req, State}.

content_types_accepted(Req, State) ->
	Handlers = [{{<<"application">>, <<"json">>, '*'}, from_json}],
	{Handlers, Req, State}.

allowed_methods(Req, State) ->
	Methods = [<<"GET">>, <<"POST">>, <<"OPTIONS">>],
	{Methods, Req, State}.

options(Req0, State) ->
	Hs =
		case cowboy_req:header(<<"access-control-request-method">>, Req0) of
			<<"POST">> -> <<"authorization, content-length, content-type">>;
			_          -> <<"authorization">>
		end,
	Req1 = cowboy_req:set_resp_header(<<"access-control-allow-methods">>, <<"GET, POST">>, Req0),
	Req2 = cowboy_req:set_resp_header(<<"access-control-allow-headers">>, Hs, Req1),
	Req3 = cowboy_req:set_resp_header(<<"access-control-allow-credentials">>, <<"true">>, Req2),
	{ok, Req3, State}.

%% =============================================================================
%% Content callbacks
%% =============================================================================

from_json(Req0, #state{bucket = Bucket, key = Key, rdesc = Rdesc} =State) ->
	datastore_http:handle_payload(Req0, State, fun(Payload, Req1) ->
		Rdata = datastore_acl:parse_resources(jsx:decode(Payload)),
		datastore_http:handle_response(Req1, State, fun() ->
			datastore_acl:update_list(Bucket, Key, Rdata, Rdesc)
		end)
	end).

to_json(Req, #state{bucket = Bucket, key = Key, rdesc = Rdesc} =State) ->
	datastore_http:handle_response(Req, State, fun() ->
		jsx:encode(datastore_acl:list(Bucket, Key, Rdesc))
	end).
