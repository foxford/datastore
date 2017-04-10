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
dep_riaks2c = git git://github.com/manifest/riak-s2-erlang-client.git v0.2.1
dep_riakacl = git git://github.com/manifest/riak-acl.git v0.1.0
dep_jose = git git://github.com/manifest/jose-erlang.git v0.1.1
dep_cowboy = git git://github.com/ninenines/cowboy.git 10dfd8c910bce3ae71146f8f13eef25b34ec093a

TEST_DEPS = ct_helper
dep_ct_helper = git git://github.com/ninenines/ct_helper.git master

SHELL_DEPS = tddreloader
SHELL_OPTS = \
	-eval 'application:ensure_all_started($(PROJECT), permanent)' \
	-s tddreloader start \
	-config rel/sys

include erlang.mk

export DEVELOP_ENVIRONMENT = $(shell if [ -f .develop-environment ]; then cat .develop-environment; fi)
export RIAKACL_DEVELOP_ENVIRONMENT = $(shell if [ -f deps/riakacl/.develop-environment ]; then cat deps/riakacl/.develop-environment; fi)
