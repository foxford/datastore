-module(datastore_acl).

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

