-module(datastore_httph_acls).

-include("datastore_log.hrl").

%% API
-export([
	handle_create/4,
	handle_read/3,
	from_json/2,
	to_json/2
]).

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

%% Types
-record(state, {
	r                  :: map(),
	authconf           :: map(),
	bucket             :: iodata(),
	key    = undefined :: iodata(),
	authm  = #{}       :: map()
}).

%% =============================================================================
%% API
%% =============================================================================

-spec handle_create(binary(), binary(), [{binary(), riakacl_group:group()}], map()) -> [map()].
handle_create(Bucket, Key, Groups, Resources) ->
	#{object_aclobject := #{pool := Pool, bucket := Ob}} = Resources,
	Okey = datastore:aclobject_key(Bucket, Key),
	Pid = gunc_pool:lock(Pool),
	E = riakacl_entry:put_groups(Pid, Ob, Okey, Groups, [return_body]),
	gunc_pool:unlock(Pool, Pid),
	groups(E).

-spec handle_read(binary(), binary(), map()) -> [map()].
handle_read(Bucket, Key, Resources) ->
	#{object_aclobject := #{pool := Pool, bucket := Ob}} = Resources,
	Okey = datastore:aclobject_key(Bucket, Key),
	Pid = gunc_pool:lock(Pool),
	E = riakacl_entry:get(Pid, Ob, Okey),
	gunc_pool:unlock(Pool, Pid),
	groups(E).

from_json(Req0, #state{bucket = Bucket, key = Key, r = Resources} =State) ->
	datastore_http:handle_payload(Req0, State, fun(Payload, Req1) ->
		datastore_http:handle_response(Req1, State, fun() ->
			handle_create(Bucket, Key, parse_groups(jsx:decode(Payload)), Resources)
		end)
	end).

to_json(Req, #state{bucket = Bucket, key = Key, r = Resources} =State) ->
	datastore_http:handle_response(Req, State, fun() ->
		jsx:encode(handle_read(Bucket, Key, Resources))
	end).

%% =============================================================================
%% REST handler callbacks
%% =============================================================================

init(Req, Opts) ->
	#{authentication := AuthConf, resources := R} = Opts,
	State =
		#state{
			r = R,
			authconf = AuthConf,
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

forbidden(Req, #state{r = Resources, bucket = Bucket, authm = AuthM} =State) ->
	try datastore:authorize(Bucket, AuthM, Resources) of
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
	Req1 = cowboy_req:set_resp_header(<<"access-control-allow-methods">>, <<"GET, POST">>, Req0),
	Req2 = cowboy_req:set_resp_header(<<"access-control-allow-headers">>, <<"Authorization, Content-Type">>, Req1),
	Req3 = cowboy_req:set_resp_header(<<"access-control-allow-credentials">>, <<"true">>, Req2),
	{ok, Req3, State}.

%% =============================================================================
%% Internal functions
%% =============================================================================

-spec groups(riakacl_entry:entry()) -> [map()].
groups(E) ->
	riakacl_entry:fold_groups_dt(
		fun(Name, Raw, Acc) ->
		Data =
			riakacl_group:parse_rawdt(
				Raw,
				fun(Data, M) ->
					{_, Val} = lists:keyfind({<<"access">>, register}, 1, Data),
					M#{access => Val}
				end),
			[#{id => Name, data => Data}|Acc]
		end, [], E).

-spec parse_groups([[{binary(), any()}]]) -> [{binary(), riakacl_group:group()}].
parse_groups(L) ->
	lists:foldl(fun(Val, Acc) -> [parse_group(Val)|Acc] end, [], L).

-spec parse_group([{binary(), any()}]) -> {binary(), riakacl_group:group()}.
parse_group(L) ->
	{_, Id} = lists:keyfind(<<"id">>, 1, L),
	{_, Data} = lists:keyfind(<<"data">>, 1, L),
	{Id, parse_group_data(Data)}.

-spec parse_group_data([{binary(), any()}]) -> riakacl_group:group().
parse_group_data(L) ->
	parse_group_data(L, undefined, #{}).

-spec parse_group_data([{binary(), any()}], binary() | undefined, map()) -> riakacl_group:group().
parse_group_data([{<<"access">>, Val}|T], _Access, Claims) -> parse_group_data(T, Val, Claims);
parse_group_data([{<<"exp">>, Val}|T], Access, Claims)     -> parse_group_data(T, Access, Claims#{exp => Val});
parse_group_data([_|T], Access, Claims)                    -> parse_group_data(T, Access, Claims);
parse_group_data([], undefined, _Claims)                   -> throw(missing_access);
parse_group_data([], Access, Claims)                       -> riakacl_rwaccess:new_dt(Access, Claims).

