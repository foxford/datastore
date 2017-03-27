-module(datastore_acl).

%% CRUD API
-export([
	list/3,
	update_list/4,
	read/4,
	read/5,
	update/6,
	delete/5
]).

%% API
-export([
	to_map/2,
	parse_resources/1,
	parse_resource/1,
	parse_resource_data/1,
	object_key/2
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
	Pid = gunc_pool:lock(Pool),
	MaybeE = riakacl_entry:find(Pid, Ob, object_key(Bucket, Key)),
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
	Pid = gunc_pool:lock(Pool),
	E = riakacl_entry:put_groups(Pid, Ob, object_key(Bucket, Key), Groups, [return_body]),
	gunc_pool:unlock(Pool, Pid),
	format_resources(E).

-spec read(binary(), binary(), binary(), map()) -> {ok, rbox()} | error.
read(Bucket, Key, Gname, Rdesc) ->
	read(Bucket, Key, Gname, Rdesc, []).

-spec read(binary(), binary(), binary(), map(), [proplists:property()]) -> {ok, rbox()} | error.
read(Bucket, Key, Gname, Rdesc, Opts) ->
	#{object_aclobject := #{pool := Pool, bucket := Ob}} = Rdesc,
	Pid = gunc_pool:lock(Pool),
	MaybeE = riakacl_entry:find(Pid, Ob, object_key(Bucket, Key), Opts),
	gunc_pool:unlock(Pool, Pid),
	case MaybeE of
		{ok, E} -> find_group(Gname, E);
		_       -> error
	end.

-spec update(binary(), binary(), binary(), riakacl_group:group(), rbox(), map()) -> map().
update(Bucket, Key, Gname, Gdata, Rbox, Rdesc) ->
	#{object_aclobject := #{pool := Pool, bucket := Ob}} = Rdesc,
	E0 =
		case Rbox of
			undefined -> riakacl_entry:new_dt(riakacl:unix_time_us());
			_         -> Rbox#rbox.p
		end,
	Pid = gunc_pool:lock(Pool),
	E1 = riakacl_entry:put_groups(Pid, Ob, object_key(Bucket, Key), [{Gname, Gdata}], E0, [return_body]),
	gunc_pool:unlock(Pool, Pid),
	format_resource(Gname, E1).

-spec delete(binary(), binary(), binary(), rbox(), map()) -> map().
delete(Bucket, Key, Gname, #rbox{p = E} =Rbox, Rdesc) ->
	#{object_aclobject := #{pool := Pool, bucket := Ob}} = Rdesc,
	Pid = gunc_pool:lock(Pool),
	_ = riakacl_entry:remove_groups(Pid, Ob, object_key(Bucket, Key), [Gname], E, []),
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

-spec object_key(binary(), binary() | undefined) -> binary().
object_key(Bucket, undefined) -> Bucket;
object_key(Bucket, Key)       -> <<Bucket/binary, $:, Key/binary>>.

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

