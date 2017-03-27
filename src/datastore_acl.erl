-module(datastore_acl).

%% CRUD API
-export([
	list/3,
	update_list/4,
	read/4,
	update/5,
	delete/5
]).

%% API
-export([
	to_map/2,
	parse_resources/1,
	parse_resource/1,
	parse_resource_data/1
]).

%% Types
-type resource() :: [riakacl_group:rawdt()].

-record(rbox, {
	r :: resource(),
	p :: riakacl_entry:entry()
}).
-type rbox() :: #rbox{}.

%% =============================================================================
%% CRUD API
%% =============================================================================

-spec list(binary(), binary(), map()) -> [map()].
list(Bucket, Key, Rdesc) ->
	#{object_aclobject := #{pool := Pool, bucket := Ob}} = Rdesc,
	Okey = datastore:aclobject_key(Bucket, Key),
	Pid = gunc_pool:lock(Pool),
	MaybeE = riakacl_entry:find(Pid, Ob, Okey),
	gunc_pool:unlock(Pool, Pid),
	case MaybeE of
		{ok, E} -> format_resources(E);
		_       -> []
	end.

%% We have 'update_list' instead of 'create' because we want to add/update
%% a list of ACL groups by one request.
-spec update_list(binary(), binary(), [{binary(), riakacl_group:group()}], map()) -> [map()].
update_list(Bucket, Key, Groups, Rdesc) ->
	#{object_aclobject := #{pool := Pool, bucket := Ob}} = Rdesc,
	Okey = datastore:aclobject_key(Bucket, Key),
	Pid = gunc_pool:lock(Pool),
	E = riakacl_entry:put_groups(Pid, Ob, Okey, Groups, [return_body]),
	gunc_pool:unlock(Pool, Pid),
	format_resources(E).

-spec read(binary(), binary(), binary(), map()) -> {ok, rbox()} | error.
read(Bucket, Key, Gname, Rdesc) ->
	#{object_aclobject := #{pool := Pool, bucket := Ob}} = Rdesc,
	Okey = datastore:aclobject_key(Bucket, Key),
	Pid = gunc_pool:lock(Pool),
	MaybeE = riakacl_entry:find(Pid, Ob, Okey),
	gunc_pool:unlock(Pool, Pid),
	case MaybeE of
		{ok, E} -> find_group(Gname, E);
		_       -> error
	end.

-spec update(binary(), binary(), binary(), riakacl_group:group(), map()) -> map().
update(Bucket, Key, Gname, Gdata, Rdesc) ->
	#{object_aclobject := #{pool := Pool, bucket := Ob}} = Rdesc,
	Okey = datastore:aclobject_key(Bucket, Key),
	Pid = gunc_pool:lock(Pool),
	E = riakacl_entry:put_groups(Pid, Ob, Okey, [{Gname, Gdata}], [return_body]),
	gunc_pool:unlock(Pool, Pid),
	format_resource(Gname, E).

-spec delete(binary(), binary(), binary(), rbox(), map()) -> map().
delete(Bucket, Key, Gname, Rbox, Rdesc) ->
	#{object_aclobject := #{pool := Pool, bucket := Ob}} = Rdesc,
	Okey = datastore:aclobject_key(Bucket, Key),
	Pid = gunc_pool:lock(Pool),
	_ = riakacl_entry:remove_groups(Pid, Ob, Okey, [Gname]),
	gunc_pool:unlock(Pool, Pid),
	to_map(Gname, Rbox).

%% =============================================================================
%% API
%% =============================================================================

-spec to_map(binary(), rbox()) -> map().
to_map(Gname, #rbox{r = Raw}) ->
	format_resource_dt(Gname, Raw).

-spec parse_resources([[{binary(), any()}]]) -> [{binary(), riakacl_group:group()}].
parse_resources(L) ->
	lists:foldl(fun(Val, Acc) -> [parse_resource(Val)|Acc] end, [], L).

-spec parse_resource([{binary(), any()}]) -> {binary(), riakacl_group:group()}.
parse_resource(L) ->
	{_, Id} = lists:keyfind(<<"id">>, 1, L),
	{_, Data} = lists:keyfind(<<"data">>, 1, L),
	{Id, parse_resource_data(Data)}.

-spec parse_resource_data([{binary(), any()}]) -> riakacl_group:group().
parse_resource_data(L) ->
	parse_resource_data(L, undefined, #{}).

%% =============================================================================
%% Internal functions
%% =============================================================================

-spec format_resources(riakacl_entry:entry()) -> [map()].
format_resources(E) ->
	riakacl_entry:fold_groups_dt(
		fun(Name, Raw, Acc) ->
			[format_resource_dt(Name, Raw)|Acc]
		end, [], E).

-spec format_resource(binary(), riakacl_entry:entry()) -> map().
format_resource(Name, E) ->
	format_resource_dt(Name, riakacl_entry:group_rawdt(Name, E)).

-spec format_resource_dt(binary(), resource()) -> map().
format_resource_dt(Name, Raw) ->
	Data =
		riakacl_group:parse_rawdt(
			Raw,
			fun(Data, M) ->
				{_, Val} = lists:keyfind({<<"access">>, register}, 1, Data),
				M#{access => Val}
			end),
	#{id => Name, data => Data}.

-spec find_group(binary(), riakacl_entry:entry()) -> {ok, rbox()} | error.
find_group(Name, E) ->
	case riakacl_entry:find_group_rawdt(Name, E) of
		{ok, Raw} -> {ok, #rbox{r = Raw, p = E}};
		error     -> error
	end.

-spec parse_resource_data([{binary(), any()}], binary() | undefined, map()) -> riakacl_group:group().
parse_resource_data([{<<"access">>, Val}|T], _Access, Claims) -> parse_resource_data(T, Val, Claims);
parse_resource_data([{<<"exp">>, Val}|T], Access, Claims)     -> parse_resource_data(T, Access, Claims#{exp => Val});
parse_resource_data([_|T], Access, Claims)                    -> parse_resource_data(T, Access, Claims);
parse_resource_data([], undefined, _Claims)                   -> throw(missing_access);
parse_resource_data([], Access, Claims)                       -> riakacl_rwaccess:new_dt(Access, Claims).

