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

-export_type([rbox/0]).

%% =============================================================================
%% CRUD API
%% =============================================================================

-spec list(binary(), binary(), map()) -> [map()].
list(Bucket, Key, Rdesc) ->
	#{object_aclobject := #{pool := KVpool, bucket := AclOb}} = Rdesc,
	KVpid = riakc_pool:lock(KVpool),
	MaybeE = riakacl_entry:find(KVpid, AclOb, object_key(Bucket, Key)),
	riakc_pool:unlock(KVpool, KVpid),
	case MaybeE of
		{ok, E} -> format_resources(E);
		_       -> []
	end.

%% We have 'update_list' instead of 'create' because we want to add/update
%% a list of ACL groups by one request.
-spec update_list(binary(), binary(), [{binary(), riakacl_group:group()}], map()) -> [map()].
update_list(Bucket, Key, Groups, Rdesc) ->
	#{object_aclobject := #{pool := KVpool, bucket := AclOb}} = Rdesc,
	KVpid = riakc_pool:lock(KVpool),
	E = riakacl_entry:put_groups(KVpid, AclOb, object_key(Bucket, Key), Groups, [return_body]),
	riakc_pool:unlock(KVpool, KVpid),
	format_resources(E).

-spec read(binary(), binary(), binary(), map()) -> {ok, rbox()} | error.
read(Bucket, Key, AclGname, Rdesc) ->
	read(Bucket, Key, AclGname, Rdesc, []).

-spec read(binary(), binary(), binary(), map(), [proplists:property()]) -> {ok, rbox()} | error.
read(Bucket, Key, AclGname, Rdesc, Opts) ->
	#{object_aclobject := #{pool := KVpool, bucket := AclOb}} = Rdesc,
	KVpid = riakc_pool:lock(KVpool),
	MaybeE = riakacl_entry:find(KVpid, AclOb, object_key(Bucket, Key), Opts),
	riakc_pool:unlock(KVpool, KVpid),
	case MaybeE of
		{ok, E} -> find_resource(AclGname, E);
		_       -> error
	end.

-spec update(binary(), binary(), binary(), riakacl_group:group(), rbox(), map()) -> map().
update(Bucket, Key, AclGname, Gdata, Rbox, Rdesc) ->
	#{object_aclobject := #{pool := KVpool, bucket := AclOb}} = Rdesc,
	E0 = case Rbox of undefined -> riakacl_entry:new_dt(); _ -> Rbox#rbox.p end,
	KVpid = riakc_pool:lock(KVpool),
	E1 = riakacl_entry:put_groups(KVpid, AclOb, object_key(Bucket, Key), [{AclGname, Gdata}], E0, [return_body]),
	riakc_pool:unlock(KVpool, KVpid),
	format_resource(AclGname, E1).

-spec delete(binary(), binary(), binary(), rbox(), map()) -> map().
delete(Bucket, Key, AclGname, #rbox{p = E} =Rbox, Rdesc) ->
	#{object_aclobject := #{pool := KVpool, bucket := AclOb}} = Rdesc,
	KVpid = riakc_pool:lock(KVpool),
	_ = riakacl_entry:remove_groups(KVpid, AclOb, object_key(Bucket, Key), [AclGname], E, []),
	riakc_pool:unlock(KVpool, KVpid),
	to_map(AclGname, Rbox).

%% =============================================================================
%% API
%% =============================================================================

-spec to_map(binary(), rbox()) -> map().
to_map(AclGname, #rbox{r = Raw}) ->
	format_resource_dt(AclGname, Raw).

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

-spec find_resource(binary(), riakacl_entry:entry()) -> {ok, rbox()} | error.
find_resource(Name, E) ->
	case riakacl_entry:find_group_rawdt(Name, E) of
		{ok, Raw} -> {ok, #rbox{r = Raw, p = E}};
		error     -> error
	end.

%% Note: we convert expiration time from seconds to microseconds
%% because it's presented in seconds for DataStore API,
%% and in microseconds for Riak ACL library.
-spec parse_resource_data([{binary(), any()}], binary() | undefined, map()) -> riakacl_group:group().
parse_resource_data([{<<"access">>, Val}|T], _Access, Claims) -> parse_resource_data(T, Val, Claims);
parse_resource_data([{<<"exp">>, Val}|T], Access, Claims)     -> parse_resource_data(T, Access, Claims#{exp => to_us(Val)});
parse_resource_data([_|T], Access, Claims)                    -> parse_resource_data(T, Access, Claims);
parse_resource_data([], undefined, _Claims)                   -> throw(missing_access);
parse_resource_data([], Access, Claims)                       -> riakacl_rwaccess:new_dt(Access, Claims).

-spec to_us(non_neg_integer()) -> non_neg_integer().
to_us(Sec) ->
	Sec *1000000.
