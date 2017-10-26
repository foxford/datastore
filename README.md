# Datastore

[![Build Status][travis-img]][travis]

Highly available, scalable and simple to use object storage
with token based (OAuth2 Bearer Token) authorization.
It stores data and large binary objects in Riak S2 and ACL entries in Riak KV.


### How To Use

To build and start playing with the application,
execute following shell commands within different terminal tabs:

```bash
## Building the development image and running the container with Riak KV (ACL) within it.
$ cd deps/riakacl && ./run-docker.sh
## Building the development image and running the container with Riak S2 (Data) within it.
$ ./run-docker.sh
## Building the application and executing an erlang shell.
$ make app shell
```



### API

Datastore could be operated through its REST APIs:

- [Object][api-object]
- [Object ACL][api-object-acl]

To make examples in the API reference work, we need to create an account with admin permissions (in fact, in the following code we're just creating an ACL entry that makes account with the specified identifier a member of predefined `admin` ACL group).

```erlang
#{account_aclsubject := #{pool := KVpool, bucket := Sb}} = datastore:resources(),
KVpid = riakc_pool:lock(KVpool),
AccountId = <<"bad99464-9af3-40e8-b24f-969967b063b7">>,
ACLGroups = [{<<"admin">>, riakacl_group:new_dt()}],
riakacl:put_subject_groups(KVpid, Sb, AccountId, ACLGroups).
```

Next, we also need to create a bucket that is used in the examples.

```erlang
#{object := #{pool := S2pool, options := S2opts}} = datastore:resources(),
S2pid = gunc_pool:lock(S2pool),
Bucket = <<"example-bucket">>,
riaks2c_bucket:await_put(S2pid, riaks2c_bucket:put(S2pid, Bucket, S2opts)).
```

Finally, we could use the following endpoint URI and access token to explore Datastore APIs.

```bash
ENDPOINT='https://localhost:8443/api/v1'
ACCESS_TOKEN='eyJhbGciOiJFUzI1NiIsInR5cCI6IkpXVCJ9.eyJhdWQiOiJ0ZXN0LWFkbWluQGV4YW1wbGUub3JnIiwiaXNzIjoiaWRwLmV4YW1wbGUub3JnIiwic3ViIjoiYmFkOTk0NjQtOWFmMy00MGU4LWIyNGYtOTY5OTY3YjA2M2I3In0.hNhYWxAnROyJOSh4OmCHkQItLAvq3db5e9SD-q7uKnE3zN1U9rs303dQkp2ZevT33yLMHg-oAM5Zp1cMUNEzjQ'
```



### License

The source code is provided under the terms of [the MIT license][license].

[api-object]:https://github.com/foxford/datastore/blob/master/API_OBJECT.md
[api-object-acl]:https://github.com/foxford/datastore/blob/master/API_OBJECT_ACL.md
[license]:http://www.opensource.org/licenses/MIT
[travis]:https://travis-ci.org/foxford/datastore?branch=master
[travis-img]:https://secure.travis-ci.org/foxford/datastore.png?branch=master
