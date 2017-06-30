-module(datastore_httph_acl).

-include("datastore_log.hrl").

%% REST handler callbacks
-export([
	init/2,
	is_authorized/2,
	forbidden/2,
	resource_exists/2,
	delete_resource/2,
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
	aclgname           :: iodata(),
	bucket             :: iodata(),
	key    = undefined :: undefined | iodata(),
	authm  = #{}       :: map(),
	rbox   = undefined :: undefined | datastore_acl:rbox()
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
			aclgname = cowboy_req:binding(aclgname, Req),
			bucket = cowboy_req:binding(bucket, Req),
			key = cowboy_req:binding(key, Req)},
	{cowboy_rest, Req, State}.

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
forbidden(Req, #state{bucket = Bucket, authm = AuthM, rdesc = Rdesc} =State) ->
	try datastore:authorize(Bucket, AuthM, Rdesc) of
		{ok, #{write := true}} -> {false, Req, State};
		_                      -> {true, Req, State}
	catch T:R ->
		?ERROR_REPORT(datastore_http_log:format_request(Req), T, R),
		{stop, cowboy_req:reply(422, Req), State}
	end.

resource_exists(#{method := Method} =Req, #state{bucket = Bucket, key = Key, aclgname = AclGname, rdesc = Rdesc} =State) ->
	try datastore_acl:read(Bucket, Key, AclGname, Rdesc, read_options(Method)) of
		{ok, Rbox} -> {true, Req, State#state{rbox = Rbox}};
		_          -> {false, Req, State}
	catch T:R ->
		?ERROR_REPORT(datastore_http_log:format_request(Req), T, R),
		{stop, cowboy_req:reply(422, Req), State}
	end.

delete_resource(Req, #state{bucket = Bucket, key = Key, aclgname = AclGname, rbox = Rbox, rdesc = Rdesc} =State) ->
	datastore_http:handle_response(Req, State, fun() ->
		jsx:encode(datastore_acl:delete(Bucket, Key, AclGname, Rbox, Rdesc))
	end).

content_types_provided(Req, State) ->
	Handlers = [{{<<"application">>, <<"json">>, '*'}, to_json}],
	{Handlers, Req, State}.

content_types_accepted(Req, State) ->
	Handlers = [{{<<"application">>, <<"json">>, '*'}, from_json}],
	{Handlers, Req, State}.

allowed_methods(Req, State) ->
	Methods = [<<"GET">>, <<"PUT">>, <<"DELETE">>, <<"OPTIONS">>],
	{Methods, Req, State}.

options(Req0, State) ->
	Hs =
		case cowboy_req:header(<<"access-control-request-method">>, Req0) of
			<<"PUT">> -> <<"authorization, content-length, content-type">>;
			_         -> <<"authorization">>
		end,
	Req1 = cowboy_req:set_resp_header(<<"access-control-allow-methods">>, <<"GET, PUT, DELETE">>, Req0),
	Req2 = cowboy_req:set_resp_header(<<"access-control-allow-headers">>, Hs, Req1),
	Req3 = cowboy_req:set_resp_header(<<"access-control-allow-credentials">>, <<"true">>, Req2),
	{ok, Req3, State}.

%% =============================================================================
%% Content callbacks
%% =============================================================================

from_json(Req0, #state{bucket = Bucket, key = Key, aclgname = AclGname, rdesc = Rdesc, rbox = Rbox} =State) ->
	datastore_http:handle_payload(Req0, State, fun(Payload, Req1) ->
		Rdata = datastore_acl:parse_resource_data(jsx:decode(Payload)),
		datastore_http:handle_response(Req1, State, fun() ->
			datastore_acl:update(Bucket, Key, AclGname, Rdata, Rbox, Rdesc)
		end)
	end).

to_json(Req, #state{aclgname = AclGname, rbox = Rbox} =State) ->
	datastore_http:handle_response(Req, State, fun() ->
		jsx:encode(datastore_acl:to_map(AclGname, Rbox))
	end).

%% =============================================================================
%% Internal functions
%% =============================================================================

%% We use strict quorum (pr=quorum) for create or update operations
%% on ACL entries and sloppy quorum for read operations.
-spec read_options(binary()) -> [proplists:property()].
read_options(<<"GET">>)    -> [];
read_options(<<"PUT">>)    -> [{pr, quorum}];
read_options(<<"DELETE">>) -> [{pr, quorum}].
