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

-module(object_SUITE).
-include_lib("common_test/include/ct.hrl").

-compile(export_all).

%% =============================================================================
%% Common Test callbacks
%% =============================================================================

all() ->
	application:ensure_all_started(datastore),
	[{group, object}].

groups() ->
	[{object, [parallel], ct_helper:all(?MODULE)}].

init_per_suite(Config) ->
	datastore_cth:init_config() ++ Config.

init_per_testcase(_Test, Config) ->
	#{object := #{pool := S2pool, options := S2opts},
		object_aclobject := #{pool := KVpool, bucket := AclObjBucket}} = datastore:resources(),
	Bucket = iolist_to_binary(datastore_cth:make_bucket()),
	BucketNoObjects = iolist_to_binary(datastore_cth:make_bucket()),
	Key = iolist_to_binary(datastore_cth:make_key()),
	Raccess = riakacl_rwaccess:new_dt(#{read => true}),
	Waccess = riakacl_rwaccess:new_dt(#{write => true}),

	%% Creating bucket and object
	S2pid = gunc_pool:lock(S2pool),
	riaks2c_bucket:await_put(S2pid, riaks2c_bucket:put(S2pid, Bucket, S2opts)),
	riaks2c_bucket:await_put(S2pid, riaks2c_bucket:put(S2pid, BucketNoObjects, S2opts)),
	riaks2c_object:await_put(S2pid, riaks2c_object:put(S2pid, Bucket, Key, <<42>>, S2opts)),
	gunc_pool:unlock(S2pool, S2pid),

	%% Setting up ACL groups
	KVpid = riakc_pool:lock(KVpool),
	riakacl:put_object_acl(KVpid, AclObjBucket, Bucket, [{<<"bucket.reader">>, Raccess}, {<<"bucket.writer">>, Waccess}]),
	riakacl:put_object_acl(KVpid, AclObjBucket, datastore_acl:object_key(Bucket, Key), [{<<"object.reader">>, Raccess}, {<<"object.writer">>, Waccess}]),
	riakc_pool:unlock(KVpool, KVpid),

	[{bucket, Bucket}, {bucket_noobjects, BucketNoObjects}, {key, Key} | Config].

end_per_testcase(_Test, Config) ->
	Config.

end_per_suite(Config) ->
	Config.

%% =============================================================================
%% Tests
%% =============================================================================

%% Returns a list of metadata for objects.
%% An empty list is returned if specified bucket doesn't exist.
list(Config) ->
	Bucket = ?config(bucket, Config),
	BucketNoObjects = ?config(bucket_noobjects, Config),
	BucketNotExist = datastore_cth:make_bucket(),
	AuthorizationH = datastore_cth:authorization_header(admin, Config),
	Test =
		[	%% bucket exist
			{200, [<<"/api/v1/buckets/">>, Bucket, <<"/objects/">>]},
			%% bucket empty 
			{200, [<<"/api/v1/buckets/">>, BucketNoObjects, <<"/objects/">>]},
			%% bucket doesn't exist
			{200, [<<"/api/v1/buckets/">>, BucketNotExist, <<"/objects/">>]} ],
	
	Pid = datastore_cth:gun_open(Config),
	[begin	
		Ref = gun:request(Pid, <<"GET">>, Path, [AuthorizationH]),
		{St, _Hs, L} = datastore_cth:gun_await_json(Pid, Ref),
		[#{<<"id">> := _, <<"data">> := _} =Obj || Obj <- L]
	end || {St, Path} <- Test].

%% Returns a filtered list of metadata for objects.
list_qs(Config) ->
	Bucket = ?config(bucket, Config),
	<<K:1/binary, _/binary>> = ?config(key, Config),
	AuthorizationH = datastore_cth:authorization_header(admin, Config),
	Path = [<<"/api/v1/buckets/">>, Bucket, <<"/objects/">>],
	Test =
		[ {200, <<"?rows=1">>},
			{200, <<"?rows=0">>},
			{400, <<"?rows=101">>},
			{200, <<"?lexprefix=", K/binary>>},
			{400, <<"?lexprefix=">>},
			{200, <<"?lexmarker=", K/binary>>},
			{400, <<"?lexmarker=">>},
			{200, <<"?rows=1&lexprefix=", K/binary, "&lexmarker=", K/binary>>} ],
	
	Pid = datastore_cth:gun_open(Config),
	[begin
		Ref = gun:request(Pid, <<"GET">>, [Path, Qs], [AuthorizationH]),
		{St, _Hs, _Body} = datastore_cth:gun_await(Pid, Ref)
	end || {St, Qs} <- Test].

%% Access is granted only for accounts w/ read permissions to the bucket,
%% and members of 'admin' (predefined) group.
list_permissions(Config) ->
	Bucket = ?config(bucket, Config),
	Allowed = [bucket_reader, admin],
	Forbidden = datastore_cth:accounts() -- Allowed,
	Path = [<<"/api/v1/buckets/">>, Bucket, <<"/objects/">>],
	Test = [{200, Allowed}, {403, Forbidden}],

	Pid = datastore_cth:gun_open(Config),
	[begin
		[begin
			Ref = gun:request(Pid, <<"GET">>, Path, datastore_cth:authorization_headers(A, Config)),
			{St, _Hs, _Body} = datastore_cth:gun_await(Pid, Ref)
		end || A <- As]
	end || {St, As} <- Test].

%% Returns context of the specified object.
%% Returns 200 'Ok' status code on success and
%% 404 'Not Found' status code when the bucket doesn't exist.
read(Config) ->
	Bucket = ?config(bucket, Config),
	BucketNotExist = datastore_cth:make_bucket(),
	Key = ?config(key, Config),
	AuthorizationH = datastore_cth:authorization_header(admin, Config),
	Test =
		[	%% bucket exist
			{200, [<<"/api/v1/buckets/">>, Bucket, <<"/objects/">>, Key]},
			%% bucket doesn't exist
			{404, [<<"/api/v1/buckets/">>, BucketNotExist, <<"/objects/">>, Key]} ],
	
	Pid = datastore_cth:gun_open(Config),
	[begin	
		Ref = gun:request(Pid, <<"GET">>, Path, [AuthorizationH]),
		{St, _Hs, _Body} = datastore_cth:gun_await(Pid, Ref)
	end || {St, Path} <- Test].

%% Access is granted only for accounts w/ read permissions to the object,
%% and members of 'admin' (predefined) group.
read_permissions(Config) ->
	Bucket = ?config(bucket, Config),
	Key = ?config(key, Config),
	Allowed = [object_reader, admin],
	Forbidden = datastore_cth:accounts() -- Allowed,
	Path = [<<"/api/v1/buckets/">>, Bucket, <<"/objects/">>, Key],
	Test = [{200, Allowed}, {403, Forbidden}],

	Pid = datastore_cth:gun_open(Config),
	[begin
		[begin
			Ref = gun:request(Pid, <<"GET">>, Path, datastore_cth:authorization_headers(A, Config)),
			{St, _Hs, _Body} = datastore_cth:gun_await(Pid, Ref)
		end || A <- As]
	end || {St, As} <- Test].

%% Returns headers of the specified object.
%% Returns 204 'No Content' status code on success and
%% 404 'Not Found' status code when the bucket doesn't exist.
head(Config) ->
	Bucket = ?config(bucket, Config),
	BucketNotExist = datastore_cth:make_bucket(),
	Key = ?config(key, Config),
	AuthorizationH = datastore_cth:authorization_header(admin, Config),
	Test =
		[	%% bucket exist
			{204, Bucket, [<<"/api/v1/buckets/">>, Bucket, <<"/objects/">>, Key]},
			%% bucket doesn't exist
			{404, BucketNotExist, [<<"/api/v1/buckets/">>, BucketNotExist, <<"/objects/">>, Key]} ],
	
	Pid = datastore_cth:gun_open(Config),
	[begin	
		Ref = gun:request(Pid, <<"HEAD">>, Path, [AuthorizationH]),
		{St, _Hs, _Body} = datastore_cth:gun_await(Pid, Ref)
	end || {St, Path} <- Test].

%% Access is granted only for accounts w/ read permissions to the object,
%% and members of 'admin' (predefined) group.
head_permissions(Config) ->
	Bucket = ?config(bucket, Config),
	Key = ?config(key, Config),
	Allowed = [object_reader, admin],
	Forbidden = datastore_cth:accounts() -- Allowed,
	Path = [<<"/api/v1/buckets/">>, Bucket, <<"/objects/">>, Key],
	Test = [{204, Allowed}, {403, Forbidden}],

	Pid = datastore_cth:gun_open(Config),
	[begin
		[begin
			Ref = gun:request(Pid, <<"HEAD">>, Path, datastore_cth:authorization_headers(A, Config)),
			{St, _Hs, _Body} = datastore_cth:gun_await(Pid, Ref)
		end || A <- As]
	end || {St, As} <- Test].

%% Adds or updates the specified object.
%% Returns 204 'No Content' status code on success and
%% 404 'Not Found' status code when the bucket doesn't exist.
update(Config) ->
	Bucket = ?config(bucket, Config),
	BucketNotExist = datastore_cth:make_bucket(),
	Key = iolist_to_binary(datastore_cth:make_key()),
	AuthorizationH = datastore_cth:authorization_header(admin, Config),
	Mb1 = 1024 *1024,
	Size = 32 *Mb1,
	Payload = <<0:(Size *8)>>,
	ContentLengthH = {<<"content-length">>, integer_to_binary(Size)},
	ContentType = <<"application/octet-stream">>,
	ContentTypeH = {<<"content-type">>, ContentType},
	Test =
		[	%% bucket exist
			{204, Bucket, [<<"/api/v1/buckets/">>, Bucket, <<"/objects/">>, Key]},
			%% bucket doesn't exist
			{404, BucketNotExist, [<<"/api/v1/buckets/">>, BucketNotExist, <<"/objects/">>, Key]} ],
	
	Pid = datastore_cth:gun_open(Config),
	[begin	
		Ref = gun:request(Pid, <<"PUT">>, Path, [AuthorizationH, ContentTypeH, ContentLengthH], Payload),
		{St, _Hs, _Body} = datastore_cth:gun_await(Pid, Ref),

		%% Checking content and headers 
		#{object := #{pool := S2pool, options := S2opts}} = datastore:resources(),
		S2pid = gunc_pool:lock(S2pool),
		case St of
			204 ->
				S2ref = riaks2c_object:get(S2pid, B, Key, S2opts),
				{200, Hs} = riaks2c_object:expect_head(S2pid, S2ref),
				{_, ContentType} = lists:keyfind(<<"content-type">>, 1, Hs),
				Payload = riaks2c_object:expect_body(S2pid, S2ref);
			404 ->
				{error, _} = riaks2c_object:await_get(S2pid, riaks2c_object:get(S2pid, B, Key, S2opts))
		end,
		gunc_pool:unlock(S2pool, S2pid)
	end || {St, B, Path} <- Test].

%% List of ACL groups can be passed along with creating/updating resource.
update_with_aclgroups(Config) ->
	Bucket = ?config(bucket, Config),
	Key = iolist_to_binary(datastore_cth:make_key()),
	AclGname = <<"test">>,
	AclH = {<<"x-datastore-acl">>, jsx:encode([#{id => AclGname, data => #{access => <<"--">>}}])},
	AuthorizationH = datastore_cth:authorization_header(admin, Config),
	Path = [<<"/api/v1/buckets/">>, Bucket, <<"/objects/">>, Key],

	Pid = datastore_cth:gun_open(Config),
	Ref = gun:request(Pid, <<"PUT">>, Path, [AuthorizationH, AclH], <<42>>),
	{204, _Hs, _Body} = datastore_cth:gun_await(Pid, Ref),

	%% Checking ACL
	#{object_aclobject := #{pool := KVpool, bucket := AclObjBucket}} = datastore:resources(),
	KVpid = riakc_pool:lock(KVpool),
	datastore_cth:has_aclgroup(KVpid, AclObjBucket, datastore_acl:object_key(Bucket, Key), AclGname),
	riakc_pool:unlock(KVpool, KVpid).

%% Access is granted only for accounts w/ write permissions to the bucket,
%% and members of 'admin' (predefined) group.
update_permissions(Config) ->
	Bucket = ?config(bucket, Config),
	Key = ?config(key, Config),
	Allowed = [bucket_writer, admin],
	Forbidden = datastore_cth:accounts() -- Allowed,
	Path = [<<"/api/v1/buckets/">>, Bucket, <<"/objects/">>, Key],
	Test = [{204, Allowed}, {403, Forbidden}],

	Pid = datastore_cth:gun_open(Config),
	[begin
		[begin
			Ref = gun:request(Pid, <<"PUT">>, Path, datastore_cth:authorization_headers(A, Config), <<42>>),
			{St, _Hs, _Body} = datastore_cth:gun_await(Pid, Ref)
		end || A <- As]
	end || {St, As} <- Test].

%% Removes the specified object and object's ACL (default behavior).
%% Returns 204 'No Content' status code on success and
%% 404 'Not Found' status code when the bucket doesn't exist.
delete(Config) ->
	Bucket = ?config(bucket, Config),
	BucketNotExist = iolist_to_binary(datastore_cth:make_bucket()),
	Key = ?config(key, Config),
	KeyNotExist = iolist_to_binary(datastore_cth:make_key()),
	AuthorizationH = datastore_cth:authorization_header(admin, Config),
	Test =
		[	%% object exist
			{204, Key, [<<"/api/v1/buckets/">>, Bucket, <<"/objects/">>, Key]},
			%% object doesn't exist
			{204, KeyNotExist, [<<"/api/v1/buckets/">>, Bucket, <<"/objects/">>, KeyNotExist]},
			%% bucket doesn't exist
			{404, Key, [<<"/api/v1/buckets/">>, BucketNotExist, <<"/objects/">>, Key]} ],

	#{object_aclobject := #{pool := KVpool, bucket := AclObjBucket}} = datastore:resources(),
	KVpid = riakc_pool:lock(KVpool),
	Pid = datastore_cth:gun_open(Config),
	[begin	
		Ref = gun:request(Pid, <<"DELETE">>, Path, [AuthorizationH]),
		{St, _Hs, _Body} = datastore_cth:gun_await(Pid, Ref),
		error = riakacl_entry:find(KVpid, AclObjBucket, datastore_acl:object_key(Bucket, K))
	end || {St, K, Path} <- Test],
	riakc_pool:unlock(KVpool, KVpid).

%% Removes the specified object but keeps object's ACL.
delete_keep_aclgroups(Config) ->
	Bucket = ?config(bucket, Config),
	Key = ?config(key, Config),
	AuthorizationH = datastore_cth:authorization_header(admin, Config),
	KeepAclH = {<<"x-datastore-keep-acl">>, <<"true">>},
	Path = [<<"/api/v1/buckets/">>, Bucket, <<"/objects/">>, Key],

	#{object_aclobject := #{pool := KVpool, bucket := AclObjBucket}} = datastore:resources(),
	KVpid = riakc_pool:lock(KVpool),
	Pid = datastore_cth:gun_open(Config),
	Ref = gun:request(Pid, <<"DELETE">>, Path, [AuthorizationH, KeepAclH]),
	{204, _Hs, _Body} = datastore_cth:gun_await(Pid, Ref),
	{ok, _} = riakacl_entry:find(KVpid, AclObjBucket, datastore_acl:object_key(Bucket, Key)),
	riakc_pool:unlock(KVpool, KVpid).

%% Access is granted only for accounts w/ write permissions to the bucket,
%% and members of 'admin' (predefined) group.
delete_permissions(Config) ->
	Bucket = ?config(bucket, Config),
	Key = ?config(key, Config),
	Allowed = [bucket_writer, admin],
	Forbidden = datastore_cth:accounts() -- Allowed,
	Path = [<<"/api/v1/buckets/">>, Bucket, <<"/objects/">>, Key],
	Test = [{204, Allowed}, {403, Forbidden}],

	Pid = datastore_cth:gun_open(Config),
	[begin
		[begin
			Ref = gun:request(Pid, <<"DELETE">>, Path, datastore_cth:authorization_headers(A, Config)),
			{St, _Hs, _Body} = datastore_cth:gun_await(Pid, Ref)
		end || A <- As]
	end || {St, As} <- Test].
