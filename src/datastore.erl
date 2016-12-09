%% ----------------------------------------------------------------------------
%% The MIT License
%%
%% Copyright (c) 2016 Andrei Nesterov <ae.nesterov@gmail.com>
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
	select_authentication_key/2,
	select_authentication_config/2
]).

%% Configuration
-export([
	http_options/0,
	httpd_acceptor_pool_size/0,
	httpd_options/0,
	allowed_origins/0,
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

%% =============================================================================
%% Configuration
%% =============================================================================

-spec http_options() -> list().
http_options() ->
	Default =
		[	{port, 8443},
			{certfile, "priv/ssl/datastore.crt"},
			{keyfile, "priv/ssl/datastore.key"} ],
	application:get_env(?APP, ?FUNCTION_NAME, Default).

-spec httpd_acceptor_pool_size() -> non_neg_integer().
httpd_acceptor_pool_size() ->
	application:get_env(?APP, ?FUNCTION_NAME, 100).

-spec httpd_options() -> map().
httpd_options() ->
	application:get_env(?APP, ?FUNCTION_NAME, #{}).

-spec allowed_origins() -> Origin | [Origin] | '*' when Origin :: {binary(), binary(), 0..65535}.
allowed_origins() ->
	application:get_env(?APP, ?FUNCTION_NAME, '*').

-spec gun_connection_pools() -> [map()].
gun_connection_pools() ->
	case application:get_env(?APP, ?FUNCTION_NAME) of
		{ok, Val} -> Val;
		_ ->
			%% Getting default values from the Docker environment
			%% configuration file, if it's available.
			{ok, Conf} = file:consult(".docker.env.config"),
			{_, #{host := Host, port := Port}} = lists:keyfind(httpc_options, 1, Conf),
			[	#{name => s2,
					size => 5,
					connection =>
						#{host => Host,
							port => Port,
							options => #{protocols => [http]}}} ]
	end.

-spec authentication() -> map().
authentication() ->
	DefaultVerifyOpts =
		#{parse_header => map,
			parse_payload => map,
			parse_signature => binary,
			verify => [exp, nbf, iat],
			leeway => 1},
	%% Examples:
	%% #{<<"iss">> =>
	%% 		#{keyfile => <<"keys/example.pem">>,
	%% 			verify_options => DefaultVerifyOpts}}
	%% #{{<<"iss">>, <<"kid">>} =>
	%% 		#{keyfile => <<"keys/example.pem">>,
	%% 			verify_options => DefaultVerifyOpts}}
	M = application:get_env(?APP, ?FUNCTION_NAME, #{}),
	try configure_auth(M, DefaultVerifyOpts)
	catch throw:R -> error({invalid_authentication_config, R, M}) end.

-spec resources() -> map().
resources() ->
	case application:get_env(?APP, ?FUNCTION_NAME) of
		{ok, Val} -> Val;
		_ ->
			%% Getting default values from the Docker environment
			%% configuration file, if it's available.
			{ok, Conf} = file:consult(".docker.env.config"),
			{_, UserOpts} = lists:keyfind(user, 1, Conf),
			#{object =>
				#{pool => s2,
					options => UserOpts}}
	end.

%% =============================================================================
%% Internal functions
%% =============================================================================

-spec configure_auth(map(), map()) -> map().
configure_auth(M, Default) ->
	maps:map(fun(_Iss, Conf) -> load_auth_key(maps:merge(Default, Conf)) end, M).

-spec load_auth_key(map()) -> map().
load_auth_key(#{alg := _, key := _} =M) -> M;
load_auth_key(#{keyfile := Path} =M) ->
	try
		{ok, Pem} = file:read_file(priv_path(Path)),
		{Alg, Key} = jose_pem:parse_key(Pem),
		M#{alg => Alg, key => Key}
	catch _:_ ->
		throw({bad_keyfile, Path})
	end;
load_auth_key(_) ->
	throw(missing_key).
