FROM ubuntu:16.04

ARG RIAKKV_VERSION
ARG RIAKCS_VERSION
ARG STANCHION_VERSION
ARG ULIMIT_FD
ENV RIAKKV_VERSION=${RIAKKV_VERSION:-2.2.0}
ENV RIAKCS_VERSION=${RIAKCS_VERSION:-2.1.1}
ENV STANCHION_VERSION=${STANCHION_VERSION:-2.1.1}
ENV ULIMIT_FD=${ULIMIT_FD:-262144}

## -----------------------------------------------------------------------------
## Installing dependencies
## -----------------------------------------------------------------------------
ENV DEBIAN_FRONTEND noninteractive
RUN set -xe \
	&& apt-get update \
	&& apt-get -y --no-install-recommends install \
		software-properties-common \
		apt-transport-https \
		ca-certificates \
		lsb-release \
		curl \
	&& apt-get update \
	&& apt-get -y --no-install-recommends install \
		rsyslog \
		vim-nox \
		sudo \
		less \
		make \
		git \
		jq

## -----------------------------------------------------------------------------
## Installing Riak KV, Riak S2, Stanchion
## -----------------------------------------------------------------------------
RUN set -xe \
	&& add-apt-repository -s -y "deb https://packagecloud.io/basho/riak/ubuntu $(lsb_release -sc) main" \
	&& add-apt-repository -s -y "deb https://packagecloud.io/basho/riak-cs/ubuntu trusty main" \
	&& add-apt-repository -s -y "deb https://packagecloud.io/basho/stanchion/ubuntu trusty main" \
	&& curl -fSL https://packagecloud.io/gpg.key 2>&1 | apt-key add -- \
	&& apt-get update \
	&& apt-get -y --no-install-recommends install \
		riak=${RIAKKV_VERSION}-1 \
		riak-cs=${RIAKCS_VERSION}-1 \
		stanchion=${STANCHION_VERSION}-1

## -----------------------------------------------------------------------------
## Configuring Riak KV
## -----------------------------------------------------------------------------
RUN set -xe \
	&& echo "ulimit -n ${ULIMIT_FD}" >> /etc/default/riak \
	&& echo "\
		[\n\
			{eleveldb, [\n\
				{total_leveldb_mem_percent, 30}\n\
			]},\n\
			{riak_kv, [\n\
				{add_paths, [\"/usr/lib/riak-cs/lib/riak_cs-${RIAKCS_VERSION}/ebin\"]},\n\
				{storage_backend, riak_cs_kv_multi_backend},\n\
				{multi_backend_prefix_list, [{<<\"0b:\">>, be_blocks}]},\n\
				{multi_backend_default, be_default},\n\
				{multi_backend, [\n\
						{be_default, riak_kv_eleveldb_backend, [\n\
								{data_root, \"/var/lib/riak/leveldb\"}\n\
						]},\n\
						{be_blocks, riak_kv_bitcask_backend, [\n\
								{data_root, \"/var/lib/riak/bitcask\"}\n\
						]}\n\
				]}\n\
			]},\n\
			{riak_core, [\n\
				{default_bucket_props, [{allow_mult, true}]}\n\
			]}\n\
		].\n\
	" > /etc/riak/advanced.config \
	&& perl -pi -e 's/(storage_backend = .*)/## ${1}/g' /etc/riak/riak.conf \
	&& perl -pi -e 's/(listener.http.internal = )127\.0\.0\.1/${1}0.0.0.0/' /etc/riak/riak.conf \
	&& perl -pi -e 's/(listener.protobuf.internal = )127\.0\.0\.1/${1}0.0.0.0/' /etc/riak/riak.conf \
	&& perl -pi -e 's/(?:(log.syslog = ).*)/${1}on/' /etc/riak/riak.conf

## -----------------------------------------------------------------------------
## Configuring Stanchion
## -----------------------------------------------------------------------------
RUN set -xe \
	&& echo "ulimit -n ${ULIMIT_FD}" >> /etc/default/stanchion \
	&& perl -pi -e 's/(listener = )127\.0\.0\.1/${1}0.0.0.0/' /etc/stanchion/stanchion.conf \
	&& perl -pi -e 's/(riak_host = )127\.0\.0\.1/${1}0.0.0.0/' /etc/stanchion/stanchion.conf

## -----------------------------------------------------------------------------
## Configuring Riak S2
## -----------------------------------------------------------------------------
RUN set -xe \
	&& echo "ulimit -n ${ULIMIT_FD}" >> /etc/default/riak-cs \
	&& echo "\
		[\n\
			{riak_cs, [\n\
				{max_buckets_per_user, 1000000}\n\
			]}\n\
		].\n\
	" > /etc/riak-cs/advanced.config \
	&& perl -pi -e 's/(listener = )127\.0\.0\.1/${1}0.0.0.0/' /etc/riak-cs/riak-cs.conf \
	&& perl -pi -e 's/(riak_host = )127\.0\.0\.1/${1}0.0.0.0/' /etc/riak-cs/riak-cs.conf \
	&& perl -pi -e 's/(stanchion_host = )127\.0\.0\.1/${1}0.0.0.0/' /etc/riak-cs/riak-cs.conf
