PROJECT = datastore
PROJECT_DESCRIPTION = Data Store

DEP_PLUGINS = \
	version.mk

DEPS = \
	lager \
	lager_syslog \
	riakc_pool \
	gunc_pool \
	riaks2c \
	riakacl \
	jose \
	cowboy \
	exometer

IGNORE_DEPS = \
	folsom \
	bear

NO_AUTOPATCH = \
	riak_pb

dep_lager = git git://github.com/erlang-lager/lager.git 3.5.1
dep_lager_syslog = git git://github.com/basho/lager_syslog.git 3.0.3
dep_riakc_pool = git git://github.com/manifest/riak-connection-pool.git v0.2.1
dep_gunc_pool = git git://github.com/manifest/gun-connection-pool.git v0.1.1
dep_riaks2c = git git://github.com/manifest/riak-s2-erlang-client.git v0.4.0
dep_riakacl = git git://github.com/manifest/riak-acl.git v0.2.0
dep_jose = git git://github.com/manifest/jose-erlang.git v0.1.2
dep_cowboy = git git://github.com/ninenines/cowboy.git 2.0.0-rc.4
dep_exometer = git git://github.com/Feuerlabs/exometer.git 1.2.1

BUILD_DEPS = version.mk
dep_version.mk = git git://github.com/manifest/version.mk.git master

TEST_DEPS = ct_helper
dep_ct_helper = git git://github.com/ninenines/ct_helper.git master

SHELL_DEPS = tddreloader
SHELL_OPTS = \
	-eval 'application:ensure_all_started($(PROJECT), permanent)' \
	-s tddreloader start \
	-config rel/sys

include erlang.mk

app:: rebar.config

export DEVELOP_ENVIRONMENT = $(shell if [ -f .develop-environment ]; then cat .develop-environment; fi)
export RIAKACL_DEVELOP_ENVIRONMENT = $(shell if [ -f deps/riakacl/.develop-environment ]; then cat deps/riakacl/.develop-environment; fi)
export EXOMETER_PACKAGES='(minimal)'
