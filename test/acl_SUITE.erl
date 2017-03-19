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

-module(acl_SUITE).
-include_lib("common_test/include/ct.hrl").

-compile(export_all).

%% =============================================================================
%% Common Test callbacks
%% =============================================================================

all() ->
	application:ensure_all_started(datastore),
	[{group, acl}].

groups() ->
	[{acl, [parallel], ct_helper:all(?MODULE)}].

init_per_suite(Config) ->
	datastore_cth:init_config() ++ Config.

init_per_testcase(_Test, Config) ->
	#{object := #{pool := S2pool, options := S2opts},
		object_aclobject := #{pool := KVpool, bucket := AclObjBucket}} = datastore:resources(),
	BucketNoGroups = iolist_to_binary(datastore_cth:make_bucket()),
	Bucket = iolist_to_binary(datastore_cth:make_bucket()),
	KeyNoGroups = <<"key_ng">>,
	Key = <<"key_1g">>,
	Raccess = riakacl_rwaccess:new_dt(#{read => true}),
	Waccess = riakacl_rwaccess:new_dt(#{write => true}),

	%% Creating 2 buckets and 2 objects
	S2pid = gunc_pool:lock(S2pool),
	_ =
		[	riaks2c_bucket:await_put(S2pid, Ref) || Ref <-
			[	riaks2c_bucket:put(S2pid, Bucket, S2opts),
				riaks2c_bucket:put(S2pid, BucketNoGroups, S2opts) ]],
	_ =
		[	riaks2c_object:await_put(S2pid, Ref) || Ref <-
			[	riaks2c_object:put(S2pid, Bucket, KeyNoGroups, <<42>>, S2opts),
				riaks2c_object:put(S2pid, Bucket, Key, <<42>>, S2opts) ]],
	gunc_pool:unlock(S2pool, S2pid),

	%% Setting up ACL groups
	KVpid = gunc_pool:lock(KVpool),
	riakacl:put_object_acl(KVpid, AclObjBucket, Bucket, [{<<"bucket.reader">>, Raccess}, {<<"bucket.writer">>, Waccess}]),
	riakacl:put_object_acl(KVpid, AclObjBucket, datastore:aclobject_key(Bucket, Key), [{<<"object.reader">>, Raccess}, {<<"object.writer">>, Waccess}]),
	gunc_pool:unlock(KVpool, KVpid),

	[{bucket, Bucket}, {bucket_nogroups, BucketNoGroups}, {object, Key}, {object_nogroups, KeyNoGroups} | Config].

end_per_testcase(Config) ->
	Config.

end_per_suite(Config) ->
	Config.

%% =============================================================================
%% Tests
%% =============================================================================

%% Returns a list of ACL groups.
%% An empty list is returned for buckets or objects that don't exist.
list(Config) ->
	Bucket = ?config(bucket, Config),
	BucketNoGroups = ?config(bucket_nogroups, Config),
	BucketNotExist = datastore_cth:make_bucket(),
	Key = ?config(object, Config),
	KeyNoGroups = ?config(object_nogroups, Config),
	KeyNotExist = datastore_cth:make_key(),
	AuthorizationH = datastore_cth:authorization_header(admin, Config),
	Test =
		[	%% bucket w/ groups
			[<<"/api/v1/buckets/">>, Bucket, <<"/acl">>],
			%% object w/ groups
			[<<"/api/v1/buckets/">>, Bucket, <<"/objects/">>, Key, <<"/acl">>],
			%% bucket w/o groups
			[<<"/api/v1/buckets/">>, BucketNoGroups, <<"/acl">>],
			%% object w/o groups
			[<<"/api/v1/buckets/">>, Bucket, <<"/objects/">>, KeyNoGroups, <<"/acl">>],
			%% bucket doesn't exist
			[<<"/api/v1/buckets/">>, BucketNotExist, <<"/acl">>],
			%% object doesn't exist
			[<<"/api/v1/buckets/">>, BucketNotExist, <<"/objects/">>, KeyNotExist, <<"/acl">>] ],

	Pid = datastore_cth:gun_open(Config),
	[begin
		Ref = gun:request(Pid, <<"GET">>, Path, [AuthorizationH]),
		{200, _Hs, L} = datastore_cth:gun_await_json(Pid, Ref),
		[#{<<"id">> := _, <<"data">> := _} =Obj || Obj <- L]
	end || Path <- Test].

%% Access is graned only for accounts w/ write permissions to the bucket,
%% and members of 'admin' (predefined) group.
list_permissions(Config) ->
	Bucket = ?config(bucket, Config),
	Key = ?config(object, Config),
	Allowed = [bucket_writer, admin],
	Status = fun(A) -> case lists:member(A, Allowed) of true -> 200; _ -> 403 end end, 
	Test =
		[	[<<"/api/v1/buckets/">>, Bucket, <<"/acl">>],
			[<<"/api/v1/buckets/">>, Bucket, <<"/objects/">>, Key, <<"/acl">>] ],

	Pid = datastore_cth:gun_open(Config),
	[begin
		St = Status(A),
		Ref = gun:request(Pid, <<"GET">>, Path, [datastore_cth:authorization_header(A, Config)]),
		{St, _Hs, _Body} = datastore_cth:gun_await(Pid, Ref)
	end || Path <- Test, A <- datastore_cth:accounts()].

%% Adds or updates a list of ACL groups.
%% Returns the modified list of ACL groups.
%% ACL groups can be added or updated for buckets or objects that haven't exist yet.
update_list(Config) ->
	Bucket = ?config(bucket, Config),
	BucketNoGroups = ?config(bucket_nogroups, Config),
	BucketNotExist = datastore_cth:make_bucket(),
	Key = ?config(object, Config),
	KeyNoGroups = ?config(object_nogroups, Config),
	KeyNotExist = datastore_cth:make_key(),
	AuthorizationH = datastore_cth:authorization_header(admin, Config),
	ContentTypeH = {<<"content-type">>, <<"application/json">>},
	GroupId = <<"test-group">>,
	GroupAccess = <<"--">>,
	Payload = jsx:encode([#{id => GroupId, data => #{access => GroupAccess, <<"exp">> => 32503680000}}]),
	Test =
		[	%% bucket w/ groups
			[<<"/api/v1/buckets/">>, Bucket, <<"/acl">>],
			%% object w/ groups
			[<<"/api/v1/buckets/">>, Bucket, <<"/objects/">>, Key, <<"/acl">>],
			%% bucket w/o groups
			[<<"/api/v1/buckets/">>, BucketNoGroups, <<"/acl">>],
			%% object w/o groups
			[<<"/api/v1/buckets/">>, Bucket, <<"/objects/">>, KeyNoGroups, <<"/acl">>],
			%% bucket doesn't exist
			[<<"/api/v1/buckets/">>, BucketNotExist, <<"/acl">>],
			%% object doesn't exist
			[<<"/api/v1/buckets/">>, BucketNotExist, <<"/objects/">>, KeyNotExist, <<"/acl">>] ],

	Pid = datastore_cth:gun_open(Config),
	[begin
		Ref = gun:request(Pid, <<"POST">>, Path, [AuthorizationH, ContentTypeH], Payload),
		{200, _Hs, L} = datastore_cth:gun_await_json(Pid, Ref),
		true =
			lists:any(
				fun(#{<<"id">> := Id, <<"data">> := #{<<"access">> := Access}}) ->
					Id =:= GroupId andalso Access =:= GroupAccess
				end, L)
	end || Path <- Test].

%% Access is graned only for accounts w/ write permissions to the bucket,
%% and members of 'admin' (predefined) group.
update_list_permissions(Config) ->
	Bucket = ?config(bucket, Config),
	Key = ?config(object, Config),
	ContentTypeH = {<<"content-type">>, <<"application/json">>},
	Payload = jsx:encode([#{id => <<"test-group">>, data => #{access => <<"--">>, <<"exp">> => 32503680000}}]),
	Allowed = [bucket_writer, admin],
	Status = fun(A) -> case lists:member(A, Allowed) of true -> 200; _ -> 403 end end, 
	Test =
		[	[<<"/api/v1/buckets/">>, Bucket, <<"/acl">>],
			[<<"/api/v1/buckets/">>, Bucket, <<"/objects/">>, Key, <<"/acl">>] ],

	Pid = datastore_cth:gun_open(Config),
	[begin
		St = Status(A),
		Ref = gun:request(Pid, <<"POST">>, Path, [datastore_cth:authorization_header(A, Config), ContentTypeH], Payload),
		{St, _Hs, _Body} = datastore_cth:gun_await(Pid, Ref)
	end || Path <- Test, A <- datastore_cth:accounts()].

%% Returns the specified ACL group.
%% The 404 'Not Found' error is returned for buckets or objects that haven't exist yet
%% or aren't members of the specified group.
read(Config) ->
	Bucket = ?config(bucket, Config),
	BucketNoGroups = ?config(bucket_nogroups, Config),
	BucketNotExist = datastore_cth:make_bucket(),
	Key = ?config(object, Config),
	KeyNoGroups = ?config(object_nogroups, Config),
	KeyNotExist = datastore_cth:make_key(),
	AuthorizationH = datastore_cth:authorization_header(admin, Config),
	GroupBucket = <<"bucket.reader">>,
	GroupObject = <<"object.reader">>,
	Test =
		[	%% bucket w/ groups
			{[<<"/api/v1/buckets/">>, Bucket, <<"/acl/">>, GroupBucket], 200},
			%% object w/ groups
			{[<<"/api/v1/buckets/">>, Bucket, <<"/objects/">>, Key, <<"/acl/">>, GroupObject], 200},
			%% bucket w/o groups
			{[<<"/api/v1/buckets/">>, BucketNoGroups, <<"/acl/">>, GroupBucket], 404},
			%% object w/o groups
			{[<<"/api/v1/buckets/">>, Bucket, <<"/objects/">>, KeyNoGroups, <<"/acl/">>, GroupObject], 404},
			%% bucket doesn't exist
			{[<<"/api/v1/buckets/">>, BucketNotExist, <<"/acl/">>, GroupBucket], 404},
			%% object doesn't exist
			{[<<"/api/v1/buckets/">>, BucketNotExist, <<"/objects/">>, KeyNotExist, <<"/acl/">>, GroupObject], 404} ],

	Pid = datastore_cth:gun_open(Config),
	[begin
		Group = lists:last(Path),
		Ref = gun:request(Pid, <<"GET">>, Path, [AuthorizationH]),
		case StatusCode of
			200 -> {200, _Hs, #{<<"id">> := Group, <<"data">> := _}} = datastore_cth:gun_await_json(Pid, Ref);
			404 -> {404, _Hs, <<>>} = datastore_cth:gun_await(Pid, Ref)
		end
	end || {Path, StatusCode} <- Test].

%% Access is graned only for accounts w/ write permissions to the bucket,
%% and members of 'admin' (predefined) group.
read_permissions(Config) ->
	Bucket = ?config(bucket, Config),
	Key = ?config(object, Config),
	GroupBucket = <<"bucket.reader">>,
	GroupObject = <<"object.reader">>,
	Allowed = [bucket_writer, admin],
	Status = fun(A) -> case lists:member(A, Allowed) of true -> 200; _ -> 403 end end, 
	Test =
		[	[<<"/api/v1/buckets/">>, Bucket, <<"/acl/">>, GroupBucket],
			[<<"/api/v1/buckets/">>, Bucket, <<"/objects/">>, Key, <<"/acl/">>, GroupObject] ],

	Pid = datastore_cth:gun_open(Config),
	[begin
		St = Status(A),
		Ref = gun:request(Pid, <<"GET">>, Path, [datastore_cth:authorization_header(A, Config)]),
		{St, _Hs, _Body} = datastore_cth:gun_await(Pid, Ref)
	end || Path <- Test, A <- datastore_cth:accounts()].

%% Adds or updates the specified ACL group.
%% Returns the modified ACL group.
%% ACL groups can be set for buckets or objects that haven't exist yet.
update(Config) ->
	Bucket = ?config(bucket, Config),
	BucketNoGroups = ?config(bucket_nogroups, Config),
	BucketNotExist = datastore_cth:make_bucket(),
	Key = ?config(object, Config),
	KeyNoGroups = ?config(object_nogroups, Config),
	KeyNotExist = datastore_cth:make_key(),
	AuthorizationH = datastore_cth:authorization_header(admin, Config),
	ContentTypeH = {<<"content-type">>, <<"application/json">>},
	GroupBucket = <<"bucket.reader">>,
	GroupObject = <<"object.reader">>,
	GroupAccess = <<"--">>,
	Payload = jsx:encode(#{access => GroupAccess, <<"exp">> => 32503680000}),
	Test =
		[	%% bucket w/ groups
			{[<<"/api/v1/buckets/">>, Bucket, <<"/acl/">>, GroupBucket], 200},
			%% object w/ groups
			{[<<"/api/v1/buckets/">>, Bucket, <<"/objects/">>, Key, <<"/acl/">>, GroupObject], 200},
			%% bucket w/o groups
			{[<<"/api/v1/buckets/">>, BucketNoGroups, <<"/acl/">>, GroupBucket], 201},
			%% object w/o groups
			{[<<"/api/v1/buckets/">>, Bucket, <<"/objects/">>, KeyNoGroups, <<"/acl/">>, GroupObject], 201},
			%% bucket doesn't exist
			{[<<"/api/v1/buckets/">>, BucketNotExist, <<"/acl/">>, GroupBucket], 201},
			%% object doesn't exist
			{[<<"/api/v1/buckets/">>, BucketNotExist, <<"/objects/">>, KeyNotExist, <<"/acl/">>, GroupObject], 201} ],

	Pid = datastore_cth:gun_open(Config),
	[begin
		Group = lists:last(Path),
		Ref = gun:request(Pid, <<"PUT">>, Path, [AuthorizationH, ContentTypeH], Payload),
		case StatusCode of
			St when St =:= 200; St =:= 201 -> {St, _Hs, #{<<"id">> := Group, <<"data">> := #{<<"access">> := GroupAccess}}} = datastore_cth:gun_await_json(Pid, Ref);
			404                            -> {404, _Hs, <<>>} = datastore_cth:gun_await(Pid, Ref)
		end
	end || {Path, StatusCode} <- Test].

%% Access is graned only for accounts w/ write permissions to the bucket,
%% and members of 'admin' (predefined) group.
update_permissions(Config) ->
	Bucket = ?config(bucket, Config),
	Key = ?config(object, Config),
	ContentTypeH = {<<"content-type">>, <<"application/json">>},
	GroupBucket = <<"bucket.reader">>,
	GroupObject = <<"object.reader">>,
	Payload = jsx:encode(#{access => <<"--">>, <<"exp">> => 32503680000}),
	Allowed = [bucket_writer, admin],
	Status = fun(A) -> case lists:member(A, Allowed) of true -> 200; _ -> 403 end end, 
	Test =
		[	[<<"/api/v1/buckets/">>, Bucket, <<"/acl/">>, GroupBucket],
			[<<"/api/v1/buckets/">>, Bucket, <<"/objects/">>, Key, <<"/acl/">>, GroupObject] ],

	Pid = datastore_cth:gun_open(Config),
	[begin
		St = Status(A),
		Ref = gun:request(Pid, <<"PUT">>, Path, [datastore_cth:authorization_header(A, Config), ContentTypeH], Payload),
		{St, _Hs, _Body} = datastore_cth:gun_await(Pid, Ref)
	end || Path <- Test, A <- datastore_cth:accounts()].

%% Removes the specified ACL group.
%% Returns the removed ACL group.
%% The 404 'Not Found' error is returned for buckets or objects that don't exist
%% or aren't members of the specified group.
delete(Config) ->
	Bucket = ?config(bucket, Config),
	BucketNoGroups = ?config(bucket_nogroups, Config),
	BucketNotExist = datastore_cth:make_bucket(),
	Key = ?config(object, Config),
	KeyNoGroups = ?config(object_nogroups, Config),
	KeyNotExist = datastore_cth:make_key(),
	AuthorizationH = datastore_cth:authorization_header(admin, Config),
	GroupBucket = <<"bucket.reader">>,
	GroupObject = <<"object.reader">>,
	GroupAccess = <<"r-">>,
	Test =
		[	%% bucket w/ groups
			{[<<"/api/v1/buckets/">>, Bucket, <<"/acl/">>, GroupBucket], 200},
			%% object w/ groups
			{[<<"/api/v1/buckets/">>, Bucket, <<"/objects/">>, Key, <<"/acl/">>, GroupObject], 200},
			%% bucket w/o groups
			{[<<"/api/v1/buckets/">>, BucketNoGroups, <<"/acl/">>, GroupBucket], 404},
			%% object w/o groups
			{[<<"/api/v1/buckets/">>, Bucket, <<"/objects/">>, KeyNoGroups, <<"/acl/">>, GroupObject], 404},
			%% bucket doesn't exist
			{[<<"/api/v1/buckets/">>, BucketNotExist, <<"/acl/">>, GroupBucket], 404},
			%% object doesn't exist
			{[<<"/api/v1/buckets/">>, BucketNotExist, <<"/objects/">>, KeyNotExist, <<"/acl/">>, GroupObject], 404} ],

	Pid = datastore_cth:gun_open(Config),
	[begin
		Group = lists:last(Path),
		Ref = gun:request(Pid, <<"DELETE">>, Path, [AuthorizationH]),
		case StatusCode of
			200 -> {200, _Hs, #{<<"id">> := Group, <<"data">> := #{<<"access">> := GroupAccess}}} = datastore_cth:gun_await_json(Pid, Ref);
			404 -> {404, _Hs, <<>>} = datastore_cth:gun_await(Pid, Ref)
		end
	end || {Path, StatusCode} <- Test].

%% Access is graned only for accounts w/ write permissions to the bucket,
%% and members of 'admin' (predefined) group.
delete_permissions_bucker_writer(Config) -> do_delete_permissions(200, [bucket_writer], Config).
delete_permissions_admin(Config)         -> do_delete_permissions(200, [admin], Config).
delete_permissions(Config)               -> do_delete_permissions(403, datastore_cth:accounts() -- [bucket_writer, admin], Config).

do_delete_permissions(Status, Accounts, Config) ->
	Bucket = ?config(bucket, Config),
	Key = ?config(object, Config),
	GroupBucket = <<"bucket.reader">>,
	GroupObject = <<"object.reader">>,
	Test =
		[	[<<"/api/v1/buckets/">>, Bucket, <<"/acl/">>, GroupBucket],
			[<<"/api/v1/buckets/">>, Bucket, <<"/objects/">>, Key, <<"/acl/">>, GroupObject] ],

	Pid = datastore_cth:gun_open(Config),
	[begin
		Ref = gun:request(Pid, <<"DELETE">>, Path, [datastore_cth:authorization_header(A, Config)]),
		{Status, _Hs, _Body} = datastore_cth:gun_await(Pid, Ref)
	end || Path <- Test, A <- Accounts].
