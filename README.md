# Data Store

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



### License

The source code is provided under the terms of [the MIT license][license].

[license]:http://www.opensource.org/licenses/MIT
[travis]:https://travis-ci.org/manifest/datastore?branch=master
[travis-img]:https://secure.travis-ci.org/manifest/datastore.png?branch=master
