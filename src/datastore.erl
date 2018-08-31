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

-module(datastore).

%% API
-export([
	unix_time/0,
	unix_time/1,
	unix_time_us/0,
	unix_time_us/1,
	priv_path/1,
	conf_path/1,
	authorize/3,
	decode_access_token/2,
	object_key/2,
	expires_in/0,
	cdn_redirect_options/3
]).

%% Configuration
-export([
	http_options/0,
	httpd_options/0,
	allowed_origins/0,
	preflight_request_max_age/0,
	riak_connection_pools/0,
	gun_connection_pools/0,
	authentication/0,
	resources/0
]).

%% Definitions
-define(APP, ?MODULE).

%% =============================================================================
%% API
%% =============================================================================

-spec unix_time() -> non_neg_integer().
unix_time() ->
	unix_time(erlang:timestamp()).

-spec unix_time(erlang:timestamp()) -> non_neg_integer().
unix_time({MS, S, _US}) ->
	MS * 1000000 + S.

-spec unix_time_us() -> non_neg_integer().
unix_time_us() ->
	unix_time_us(erlang:timestamp()).

-spec unix_time_us(erlang:timestamp()) -> non_neg_integer().
unix_time_us({MS, S, US}) ->
	MS * 1000000000000 + S * 1000000 + US.

-spec priv_path(binary()) -> binary().
priv_path(Path) ->
	Priv =
		case code:priv_dir(?APP) of
			{error, _} -> "priv";
			Dir        -> Dir
		end,
	<<(list_to_binary(Priv))/binary, $/, Path/binary>>.

-spec conf_path(binary()) -> binary().
conf_path(Path) ->
	case filename:pathtype(Path) of
		relative -> priv_path(Path);
		_        -> Path
	end.

-spec authorize(binary(), map(), map()) -> {ok, map()} | {error, any()}.
authorize(AclOkey, AuthM, Rdesc) ->
	#{account_aclsubject := #{bucket := AclSb, pool := KVpool},
		object_aclobject := #{bucket := AclOb},
		anonymous_aclgroup := AnonymousGroupName,
		admin_aclgroup := AdminGroupName} = Rdesc,

	AdminAccess = {AdminGroupName, #{read => true, write => true}},
	Pid = riakc_pool:lock(KVpool),
	Result =
		case maps:find(<<"sub">>, AuthM) of
			{ok, AclSkey} -> riakacl:authorize_predefined_object(Pid, AclSb, AclSkey, AclOb, AclOkey, [AdminAccess], [AnonymousGroupName], riakacl_rwaccess);
			error         -> riakacl:authorize_predefined_subject(Pid, [AnonymousGroupName], AclOb, AclOkey, riakacl_rwaccess)
		end,
	riakc_pool:unlock(KVpool, Pid),
	Result.

-spec decode_access_token(binary(), map()) -> map().
decode_access_token(Token, AuthConf) ->
	jose_jws_compact:decode_fn(
		fun(Data, _Opts) -> select_authentication_key(Data, AuthConf) end,
		Token).

-spec object_key(iodata(), iodata()) -> iodata().
object_key(undefined, Key) -> Key;
object_key(Set, Key)       -> [Set, <<$.>>, Key].

-spec expires_in() -> non_neg_integer().
expires_in() ->
	300. %% 5 minutes

-spec cdn_redirect_options(binary(), binary(), map()) -> map().
cdn_redirect_options(Bucket, Set, Rdesc) ->
	#{object := #{cdn_sets := Sets, cdn_buckets := Buckets, cdn_redirect := ReadRedirect, redirect := WriteRedirect}} = Rdesc,
	case lists:member(Set, Sets) of
		true ->
			case lists:member(Bucket, Buckets) of
				true -> ReadRedirect;
				_    -> WriteRedirect
			end;
		_ ->
			WriteRedirect
	end.

%% =============================================================================
%% Configuration
%% =============================================================================

-spec http_options() -> list().
http_options() ->
	Default =
		[	{port, 8443},
			{certfile, conf_path(<<"ssl/datastore.crt">>)},
			{keyfile, conf_path(<<"ssl/datastore.key">>)} ],
	application:get_env(?APP, ?FUNCTION_NAME, Default).

-spec httpd_options() -> map().
httpd_options() ->
	application:get_env(?APP, ?FUNCTION_NAME, #{}).

-spec allowed_origins() -> Origin | [Origin] | '*' when Origin :: {binary(), binary(), 0..65535}.
allowed_origins() ->
	application:get_env(?APP, ?FUNCTION_NAME, '*').

-spec preflight_request_max_age() -> binary().
preflight_request_max_age() ->
	application:get_env(?APP, ?FUNCTION_NAME, <<"0">>).

-spec riak_connection_pools() -> [map()].
riak_connection_pools() ->
	case application:get_env(?APP, ?FUNCTION_NAME) of
		{ok, Val} -> Val;
		_ ->
			%% Getting default values from the Docker environment
			%% variable, if it's available.
			try
				{ok, S, _} = erl_scan:string(os:getenv("RIAKACL_DEVELOP_ENVIRONMENT")),
				{ok, Conf} = erl_parse:parse_term(S),
				#{kv_protobuf := #{host := Host, port := Port}} = Conf,
				[	#{name => kv_protobuf,
						size => 10,
						connection =>
							#{host => Host,
								port => Port,
								options => [queue_if_disconnected]}} ]
			catch _:Reason -> error({missing_develop_environment, ?FUNCTION_NAME, Reason}) end
	end.

-spec gun_connection_pools() -> [map()].
gun_connection_pools() ->
	case application:get_env(?APP, ?FUNCTION_NAME) of
		{ok, Val} -> Val;
		_ ->
			%% Getting default values from the Docker environment
			%% variable, if it's available.
			try
				{ok, S, _} = erl_scan:string(os:getenv("DEVELOP_ENVIRONMENT")),
				{ok, Conf} = erl_parse:parse_term(S),
				#{s2_http := #{host := Host, port := Port}} = Conf,
				[	#{name => s2_http,
						size => 0,
						connection =>
							#{host => Host,
								port => Port,
								options => #{protocols => [http], http_opts => #{keepalive => infinity}}}} ]
			catch _:Reason -> error({missing_develop_environment, ?FUNCTION_NAME, Reason}) end
	end.

-spec authentication() -> map().
authentication() ->
	%% Examples:
	%% #{<<"iss">> =>
	%% 		#{keyfile => <<"keys/example.pem">>,
	%% 			verify_options => DefaultVerifyOpts}}
	%% #{{<<"iss">>, <<"kid">>} =>
	%% 		#{keyfile => <<"keys/example.pem">>,
	%% 			verify_options => DefaultVerifyOpts}}
	DevelopConf =
		#{<<"idp.example.org">> =>
				#{keyfile => conf_path(<<"keys/idp-example.pub.pem">>),
					verify_options => #{verify => [exp]}}},
	DefaultVerifyOpts =
		#{parse_header => map,
			parse_payload => map,
			parse_signature => binary,
			verify => [exp, nbf, iat],
			leeway => 1},
	Default = #{},
	M =
		case application:get_env(?APP, ?FUNCTION_NAME) of
			{ok, Val} -> Val;
			_ ->
				%% Getting development values, if environment variable is defined.
				case os:getenv("DEVELOP_ENVIRONMENT") of
					Env when Env =:= false; Env =:= [] -> Default;
					_                                  -> DevelopConf
				end
		end,
	try configure_auth(M, DefaultVerifyOpts)
	catch throw:R -> error({invalid_authentication_config, R, M}) end.

-spec resources() -> map().
resources() ->
	case application:get_env(?APP, ?FUNCTION_NAME) of
		{ok, Val} -> Val;
		_ ->
			%% Getting default values from the Docker environment
			%% variable, if it's available.
			try
				{ok, S, _} = erl_scan:string(os:getenv("DEVELOP_ENVIRONMENT")),
				{ok, Conf} = erl_parse:parse_term(S),
				#{s2_user := UserOpts} = Conf,
				#{object =>
						#{pool => s2_http,
							cdn_sets => [<<"cdn-test-set">>],
							cdn_buckets => [<<"cdn-test-bucket">>],
							cdn_redirect => #{host => <<"s4.amazonaws.com">>, port => 8080, schema => <<"http://">>},
							redirect => #{host => <<"s3.amazonaws.com">>, port => 8080, schema => <<"http://">>},
							options => UserOpts,
							lock_timeout => 5000,
							read_timeout => 60000,
							write_timeout => 60000,
							handler => datastore_objecth},
					account_aclsubject =>
						#{pool => kv_protobuf,
							bucket => {<<"riakacl_subject_t">>, <<"datastore-account-aclsubject">>}},
					object_aclobject =>
						#{pool => kv_protobuf,
							bucket => {<<"riakacl_object_t">>, <<"datastore-object-aclobject">>}},
					anonymous_aclgroup => <<"anonymous">>,
					admin_aclgroup => <<"admin">>}
			catch _:Reason -> error({missing_develop_environment, ?FUNCTION_NAME, Reason}) end
	end.

%% =============================================================================
%% Internal functions
%% =============================================================================

-spec configure_auth(map(), map()) -> map().
configure_auth(M, DefaultVerifyOpts) ->
	maps:map(
		fun(_Iss, Conf) ->
			load_auth_key(Conf#{verify_options => maps:merge(DefaultVerifyOpts, maps:get(verify_options, Conf, #{}))})
		end, M).

-spec load_auth_key(map()) -> map().
load_auth_key(#{alg := _, key := _} =M) -> M;
load_auth_key(#{keyfile := Path} =M) ->
	try
		{ok, Pem} = file:read_file(conf_path(Path)),
		{Alg, Key} = jose_pem:parse_key(Pem),
		M#{alg => Alg, key => Key}
	catch _:_ ->
		throw({bad_keyfile, Path})
	end;
load_auth_key(_) ->
	throw(missing_key).

-spec select_authentication_key(list(), map()) -> jose_jws_compact:select_key_result().
select_authentication_key([ _, #{<<"iss">> := Iss} | _ ], Conf) ->
	select_authentication_config(Iss, Conf);
select_authentication_key([ #{<<"kid">> := Kid}, #{<<"iss">> := Iss} | _ ], Conf) ->
	select_authentication_config({Iss, Kid}, Conf);
select_authentication_key(_Data, _Conf) ->
	{error, missing_access_token_iss}.

-spec select_authentication_config(binary() | {binary(), binary()}, map()) -> jose_jws_compact:select_key_result().
select_authentication_config(IssKid, Conf) ->
	case maps:find(IssKid, Conf) of
		{ok, M} ->
			#{alg := Alg, key := Key, verify_options := Opts} = M,
			{ok, {Alg, Key, Opts}};
		_ ->
			{error, {missing_authentication_config, IssKid}}
	end.
