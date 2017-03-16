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
	groups/1,
	parse_groups/1,
	group/2,
	find_group/2,
	parse_group/1,
	parse_group_data/1
]).

%% =============================================================================
%% CRUD API
%% =============================================================================

-spec list(binary(), binary(), map()) -> [map()].
list(Bucket, Key, Rdesc) ->
	#{object_aclobject := #{pool := Pool, bucket := Ob}} = Rdesc,
	Okey = datastore:aclobject_key(Bucket, Key),
	Pid = gunc_pool:lock(Pool),
	E = riakacl_entry:get(Pid, Ob, Okey),
	gunc_pool:unlock(Pool, Pid),
	groups(E).

%% We have 'update_list' instead of 'create' because we want to add/update
%% a list of ACL groups by one request.
-spec update_list(binary(), binary(), [{binary(), riakacl_group:group()}], map()) -> [map()].
update_list(Bucket, Key, Groups, Rdesc) ->
	#{object_aclobject := #{pool := Pool, bucket := Ob}} = Rdesc,
	Okey = datastore:aclobject_key(Bucket, Key),
	Pid = gunc_pool:lock(Pool),
	E = riakacl_entry:put_groups(Pid, Ob, Okey, Groups, [return_body]),
	gunc_pool:unlock(Pool, Pid),
	groups(E).

-spec read(binary(), binary(), binary(), map()) -> {ok, map()} | error.
read(Bucket, Key, Gname, Rdesc) ->
	#{object_aclobject := #{pool := Pool, bucket := Ob}} = Rdesc,
	Okey = datastore:aclobject_key(Bucket, Key),
	Pid = gunc_pool:lock(Pool),
	E = riakacl_entry:get(Pid, Ob, Okey),
	gunc_pool:unlock(Pool, Pid),
	find_group(Gname, E).

-spec update(binary(), binary(), binary(), riakacl_group:group(), map()) -> map().
update(Bucket, Key, Gname, Gdata, Rdesc) ->
	#{object_aclobject := #{pool := Pool, bucket := Ob}} = Rdesc,
	Okey = datastore:aclobject_key(Bucket, Key),
	Pid = gunc_pool:lock(Pool),
	E = riakacl_entry:put_groups(Pid, Ob, Okey, [{Gname, Gdata}], [return_body]),
	gunc_pool:unlock(Pool, Pid),
	group(Gname, E).

-spec delete(binary(), binary(), binary(), map(), map()) -> map().
delete(Bucket, Key, Gname, R, Rdesc) ->
	#{object_aclobject := #{pool := Pool, bucket := Ob}} = Rdesc,
	Okey = datastore:aclobject_key(Bucket, Key),
	Pid = gunc_pool:lock(Pool),
	_ = riakacl_entry:remove_groups(Pid, Ob, Okey, [Gname]),
	gunc_pool:unlock(Pool, Pid),
	R.

%% =============================================================================
%% API
%% =============================================================================

-spec groups(riakacl_entry:entry()) -> [map()].
groups(E) ->
	riakacl_entry:fold_groups_dt(
		fun(Name, Raw, Acc) ->
			[format_group(Name, Raw)|Acc]
		end, [], E).

-spec parse_groups([[{binary(), any()}]]) -> [{binary(), riakacl_group:group()}].
parse_groups(L) ->
	lists:foldl(fun(Val, Acc) -> [parse_group(Val)|Acc] end, [], L).

-spec group(binary(), riakacl_entry:entry()) -> map().
group(Name, E) ->
	format_group(Name, riakacl_entry:group_dt(Name, E)).

-spec find_group(binary(), riakacl_entry:entry()) -> {ok, map()} | error.
find_group(Name, E) ->
	case riakacl_entry:find_group_dt(Name, E) of
		{ok, Raw} -> {ok, format_group(Name, Raw)};
		error     -> error
	end.

-spec parse_group([{binary(), any()}]) -> {binary(), riakacl_group:group()}.
parse_group(L) ->
	{_, Id} = lists:keyfind(<<"id">>, 1, L),
	{_, Data} = lists:keyfind(<<"data">>, 1, L),
	{Id, parse_group_data(Data)}.

-spec parse_group_data([{binary(), any()}]) -> riakacl_group:group().
parse_group_data(L) ->
	parse_group_data(L, undefined, #{}).

%% =============================================================================
%% Internal functions
%% =============================================================================

-spec format_group(binary(), [riakacl_group:rawdt()]) -> map().
format_group(Name, Raw) ->
	Data =
		riakacl_group:parse_rawdt(
			Raw,
			fun(Data, M) ->
				{_, Val} = lists:keyfind({<<"access">>, register}, 1, Data),
				M#{access => Val}
			end),
	#{id => Name, data => Data}.

-spec parse_group_data([{binary(), any()}], binary() | undefined, map()) -> riakacl_group:group().
parse_group_data([{<<"access">>, Val}|T], _Access, Claims) -> parse_group_data(T, Val, Claims);
parse_group_data([{<<"exp">>, Val}|T], Access, Claims)     -> parse_group_data(T, Access, Claims#{exp => Val});
parse_group_data([_|T], Access, Claims)                    -> parse_group_data(T, Access, Claims);
parse_group_data([], undefined, _Claims)                   -> throw(missing_access);
parse_group_data([], Access, Claims)                       -> riakacl_rwaccess:new_dt(Access, Claims).

