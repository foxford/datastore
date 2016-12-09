PROJECT = datastore
PROJECT_DESCRIPTION = Data store.
PROJECT_VERSION = 0.1.0

DEPS = \
	lager \
	lager_syslog \
	riaks2c \
	gunc_pool \
	jose \
	cowboy

dep_lager_syslog = git git://github.com/basho/lager_syslog.git 3.0.3
dep_riaks2c = git git://github.com/manifest/riak-s2-erlang-client.git master
dep_gunc_pool = git git://github.com/manifest/gun-connection-pool.git master
dep_jose = git git://github.com/manifest/jose-erlang.git v0.1.1
dep_cowboy = git git://github.com/ninenines/cowboy.git 7b248e5163fd852d6defe967318da849433dadb1

SHELL_DEPS = tddreloader
SHELL_OPTS = \
	-eval 'application:ensure_all_started($(PROJECT), permanent)' \
	-s tddreloader start \
	-config rel/sys

include erlang.mk

plt-hotfix:
	$(verbose) dialyzer \
		--remove_from_plt \
		--plt .datastore.plt \
		-r "$(CURDIR)/deps/cowboy/ebin/cowboy_rest.beam" \
		| true
