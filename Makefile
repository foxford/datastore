PROJECT = datastore
PROJECT_DESCRIPTION = Data store.
PROJECT_VERSION = 0.1.0

DEPS = \
	lager \
	lager_syslog \
	riakc_pool \
	gunc_pool \
	riaks2c \
	riakacl \
	jose \
	cowboy

dep_lager_syslog = git git://github.com/basho/lager_syslog.git 3.0.3
dep_riakc_pool = git git://github.com/manifest/riak-connection-pool.git v0.2.0
dep_gunc_pool = git git://github.com/manifest/gun-connection-pool.git v0.1.0
dep_riaks2c = git git://github.com/manifest/riak-s2-erlang-client.git v0.1.0
dep_riakacl = git git://github.com/manifest/riak-acl.git master
dep_jose = git git://github.com/manifest/jose-erlang.git v0.1.1
dep_cowboy = git git://github.com/ninenines/cowboy.git 0d81dc04f1d2194027ab918835b16a9fc0bceb9b

SHELL_DEPS = tddreloader
SHELL_OPTS = \
	-eval 'application:ensure_all_started($(PROJECT), permanent)' \
	-s tddreloader start \
	-config rel/sys

include erlang.mk
