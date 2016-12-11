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

-module(datastore_http).

-include("datastore_log.hrl").

%% API
-export([
	start/0,
	access_token/1,
	find_access_token/1,
	decode_access_token/2,
	access_token_type/0,
	handle_response/3,
	handle_response/4,
	control_response/5,
	handle_payload/3,
	handle_payload/4,
	handle_payload/5,
	control_payload/5,
	encode_payload/2
]).

%% Definitions
-define(DEFAULT_CONTENT_TYPE, <<"application/json">>).
-define(AUTHORIZATION, <<"authorization">>).
-define(BEARER, <<"Bearer">>).

%% Types
-type payload() :: map() | binary().

%% =============================================================================
%% API
%% =============================================================================

-spec start() -> {ok, pid()} | {error, any()}.
start() ->
	HttpOpts = datastore:http_options(),
	HttpdRequiredOpts =
		#{stream_handler => {datastore_streamh, supervisor},
			middlewares => [datastore_httpm_cors, cowboy_router, cowboy_handler],
			env => #{dispatch => dispatch(), allowed_origins => datastore:allowed_origins()}},
	HttpdStart =
		case lists:keyfind(certfile, 1, HttpOpts) of
			{certfile, _Val} -> start_tls;
			_                -> start_clear
		end,
	cowboy:HttpdStart(
		httpd,
		datastore:httpd_acceptor_pool_size(),
		HttpOpts,
		maps:merge(HttpdRequiredOpts, datastore:httpd_options())).

-spec dispatch() -> cowboy_router:dispatch_rules().
dispatch() ->
	cowboy_router:compile(routes()).

-spec access_token_type() -> binary().
access_token_type() -> ?BEARER.

-spec find_access_token(cowboy_req:req()) -> {ok, binary()} | error.
find_access_token(Req) ->
	case cowboy_req:parse_header(?AUTHORIZATION, Req) of
		{bearer, Token} -> {ok, Token};
		_               -> error
	end.

-spec access_token(cowboy_req:req()) -> binary().
access_token(Req) ->
	case find_access_token(Req) of
		{ok, Token} -> Token;
		_           -> error(missing_access_token)
	end.

-spec decode_access_token(cowboy_req:req(), map()) -> map().
decode_access_token(Req, AuthConf) ->
	datastore:decode_access_token(access_token(Req), AuthConf).

-spec handle_response(Req, State, HandleSuccess) -> {Result, Req, State}
	when
		Req           :: cowboy_req:req(),
		State         :: any(),
		HandleSuccess :: fun(() -> ok | binary() | map()),
		Result        :: true | stop | binary().
handle_response(Req, State, Handler) ->
	handle_response(Req, State, ?DEFAULT_CONTENT_TYPE, Handler).

-spec handle_response(Req, State, ContentType, HandleSuccess) -> {Result, Req, State}
	when
		Req           :: cowboy_req:req(),
		State         :: any(),
		ContentType   :: binary(),
		HandleSuccess :: fun(() -> ok | binary() | map()),
		Result        :: true | stop | binary().
handle_response(Req, State, ContentType, HandleSuccess) ->
	HandleFailure =
		fun(_Fpayload, Freq, Fstate) ->
			{stop, cowboy_req:reply(422, Freq), Fstate}
		end,
	control_response(Req, State, ContentType, HandleSuccess, HandleFailure).

-spec control_response(Req, State, ContentType, HandleSuccess, HandleFailure) -> {Result, Req, State}
	when
		Req           :: cowboy_req:req(),
		State         :: any(),
		ContentType   :: binary(),
		HandleSuccess :: fun(() -> ok | binary() | map()),
		HandleFailure :: fun((payload(), Req, any()) -> {Result, Req, State}),
		Result        :: true | stop | binary().
control_response(Req, State, ContentType, HandleSuccess, HandleFailure) ->
	AfterSuccess =
		fun
			(<<"GET">>, Body)    -> {encode_payload(ContentType, Body), Req, State};
			(<<"HEAD">>, Body)   -> {encode_payload(ContentType, Body), Req, State};
			(_Method, ok)        -> {true, Req, State};
			(_Method, Body) ->
				Req2 = cowboy_req:set_resp_header(<<"content-type">>, ContentType, Req),
				Req3 = cowboy_req:set_resp_body(encode_payload(ContentType, Body), Req2),
				{true, Req3, State}
		end,
	try HandleSuccess() of
		Body -> AfterSuccess(maps:get(method, Req), Body)
	catch
		T:R ->
			?ERROR_REPORT(datastore_http_log:format_request(Req), T, R),
			%% TODO: provide an informative error payload as a first argument
			HandleFailure(#{}, Req, State)
	end.

-spec handle_payload(Req, State, HandleSuccess) -> {Result, Req, State}
	when
		Req             :: cowboy_req:req(),
		State           :: any(),
		HandleSuccess   :: fun((Req, any()) -> {Result, Req, State}),
		Result          :: true | stop | binary().
handle_payload(Req, State, HandleSuccess) ->
	handle_payload(Req, State, #{}, HandleSuccess).

-spec handle_payload(Req, State, ReadOpts, HandleSuccess) -> {Result, Req, State}
	when
		Req             :: cowboy_req:req(),
		State           :: any(),
		ReadOpts        :: map(),
		HandleSuccess   :: fun((Req, any()) -> {Result, Req, State}),
		Result          :: true | stop | binary().
handle_payload(Req, State, ReadOpts, HandleSuccess) ->
	handle_payload(Req, State, ReadOpts, ?DEFAULT_CONTENT_TYPE, HandleSuccess).

-spec handle_payload(Req, State, ReadOpts, RespContentType, HandleSuccess) -> {Result, Req, State}
	when
		Req             :: cowboy_req:req(),
		State           :: any(),
		ReadOpts        :: map(),
		RespContentType :: binary(),
		HandleSuccess   :: fun((Req, any()) -> {Result, Req, State}),
		Result          :: true | stop | binary().
handle_payload(Req, State, ReadOpts, RespContentType, HandleSuccess) ->
	HandleFailure =
		fun
			%% Only POST, PUT, PATCH requests have a payload
			(Fpayload, Freq0, Fstate) ->
				Freq1 = cowboy_req:set_resp_header(<<"content-type">>, RespContentType, Freq0),
				Freq2 = cowboy_req:set_resp_body(encode_payload(RespContentType, Fpayload), Freq1),
				{false, Freq2, Fstate}
		end,
	control_payload(Req, State, ReadOpts, HandleSuccess, HandleFailure).

-spec control_payload(Req, State, ReadOpts, HandleSuccess, HandleFailure) -> {Result, Req, State}
	when
		Req             :: cowboy_req:req(),
		State           :: any(),
		ReadOpts        :: map(),
		HandleSuccess   :: fun((Req, any()) -> {Result, Req, State}),
		HandleFailure   :: fun((payload(), Req, any()) -> {Result, Req, State}),
		Result          :: true | stop | binary().
control_payload(Req0, State, ReadOpts, HandleSuccess, HandleFailure) ->
	case cowboy_req:read_body(Req0, ReadOpts) of
		{ok, <<>>, Req1} ->
			HandleFailure(#{error => missing_payload}, Req1, State);
		{ok, Body, Req1} ->
			try HandleSuccess(Body, Req1)
			catch T:R ->
				?ERROR_REPORT(datastore_http_log:format_request(Req1), T, R),
				HandleFailure(#{error => bad_payload, payload => Body}, Req1, State)
			end;
		{more, _, Req1}  ->
			HandleFailure(#{error => bad_payload_length}, Req1, State)
	end.

-spec encode_payload(binary(), payload()) -> iodata().
encode_payload(_ContentType, Body) when is_binary(Body) -> Body;
encode_payload(<<"application/json", _/bits>>, Body)    -> jsx:encode(Body);
encode_payload(ContentType, _Body)                      -> error({unsupported_content_type, ContentType}).

%% =============================================================================
%% Internal functions
%% =============================================================================

-spec routes() -> list(tuple()).
routes() ->
	Opts = #{resources => datastore:resources(), authentication => datastore:authentication()},
	Objects =
		[{"/api[/v1]/buckets/:bucket/objects/:key", datastore_httph_object, Opts}],

	%Pages =
	%	[{"/api[/v1]/pages/:bucket/[...]", datastore_httph_pages, #{}}],

	[{'_', Objects}].
