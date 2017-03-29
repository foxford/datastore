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

-module(datastore_cth).

-include_lib("riakc/include/riakc.hrl").

%% API
-export([
	init_config/0,
	gun_open/1,
	gun_await/2,
	gun_await_json/2,
	gun_down/1,
	riaks2c_open/1,
	authorization_header/2,
	authorization_headers/2,
	accounts/0,
	make_bucket/0,
	make_key/0
]).

%% =============================================================================
%% API
%% =============================================================================

-spec init_config() -> list().
init_config() ->
	init_accounts([]).

-spec gun_open(list()) -> pid().
gun_open(_Config) ->
	Host = "localhost",
	{_, Port} = lists:keyfind(port, 1, datastore:http_options()),
	{ok, Pid} = gun:open(Host, Port, #{retry => 0, protocols => [http2], transport => ssl}),
	Pid.

-spec gun_down(pid()) -> ok.
gun_down(Pid) ->
	receive {gun_down, Pid, _, _, _, _} -> ok
	after 500 -> error(timeout) end.

-spec gun_await(pid(), reference()) -> {100..999, [{binary(), iodata()}], binary()}.
gun_await(Pid, Ref) ->
	case gun:await(Pid, Ref) of
		{response, fin, St, Hs}   -> {St, Hs, <<>>};
		{response, nofin, St, Hs} ->
			{ok, Body} = gun:await_body(Pid, Ref),
			{St, Hs, Body}
	end.

-spec gun_await_json(pid(), reference()) -> {100..999, [{binary(), iodata()}], map()}.
gun_await_json(Pid, Ref) ->
	{St, Hs, Body} = gun_await(Pid, Ref),
	try {St, Hs, jsx:decode(Body, [return_maps, strict])}
	catch _:_ -> error({bad_response, {St, Hs, Body}}) end.

-spec riaks2c_open(list()) -> pid().
riaks2c_open(Config) ->
	#{host := Host,
		port := Port} = ct_helper:config(s2_http, Config),
	{ok, Pid} = gun:open(Host, Port, #{retry => 0, protocols => [http]}),
	Pid.

-spec authorization_headers(atom(), list()) -> [{binary(), iodata()}].
authorization_headers(anonymous, Config) -> [];
authorization_headers(Account, Config)   -> [authorization_header(Account, Config)].

-spec authorization_header(atom(), list()) -> {binary(), iodata()}.
authorization_header(Account, Config) ->
	{_, #{access_token := Token}} = lists:keyfind(Account, 1, Config),
	{<<"authorization">>, [<<"Bearer ">>, Token]}.

-spec accounts() -> [atom()].
accounts() ->
	[bucket_reader, bucket_writer, object_reader, object_reader, admin, anonymous].

%% A bucket name must obey the following rules, which produces a DNS-compliant bucket name:
%% - Must be from 3 to 63 characters.
%% - Must be one or more labels, each separated by a period (.). Each label:
%% - Must start with a lowercase letter or a number. Must end with a lowercase letter or a number. Can contain lowercase letters, numbers and dashes.
%% - Must not be formatted as an IP address (e.g., 192.168.9.2).
%% https://docs.basho.com/riak/cs/2.1.1/references/apis/storage/s3/put-bucket
-spec make_bucket() -> iodata().
make_bucket() ->
	Uniq = integer_to_binary(erlang:unique_integer([positive])),
	Size = rand:uniform(61) -byte_size(Uniq),
	[	<<(oneof(alphanum_lowercase_chars()))>>,
		Uniq,
		list_to_binary(vector(Size, [$-|alphanum_lowercase_chars()])),
		<<(oneof(alphanum_lowercase_chars()))>> ].

-spec make_key() -> iodata().
make_key() ->
	Uniq = integer_to_binary(erlang:unique_integer([positive])),
	Size = 255 -byte_size(Uniq),
	[Uniq, list_to_binary(vector(Size, alphanum_chars()))].

%%% =============================================================================
%%% Internal functions
%%% =============================================================================

-spec init_accounts(list()) -> list().
init_accounts(Config) ->
	#{account_aclsubject := #{pool := KVpool, bucket := AclSb}} = datastore:resources(),
	{ok, Pem} = file:read_file(datastore:conf_path(<<"keys/idp-example.priv.pem">>)),
	{Alg, Priv} = jose_pem:parse_key(Pem),
	CreateAccount =
		fun(Pid, Akey, Groups) ->
			Token =
				jose_jws_compact:encode(
					#{iss => <<"idp.example.org">>,
						aud => <<"app.example.org">>,
						exp => 32503680000,
						sub => Akey},
					Alg,
					Priv),
			riakacl:put_subject_groups(Pid, AclSb, Akey, Groups),
			#{id => Akey, access_token => Token}
		end,

	KVpid = riakc_pool:lock(KVpool),
	AccountsConf =
		[	{bucket_reader, CreateAccount(KVpid, <<"bucket.reader">>, [{<<"bucket.reader">>, riakacl_group:new_dt()}])},
			{bucket_writer, CreateAccount(KVpid, <<"bucker.writer">>, [{<<"bucket.writer">>, riakacl_group:new_dt()}])},
			{object_reader, CreateAccount(KVpid, <<"object.reader">>, [{<<"object.reader">>, riakacl_group:new_dt()}])},
			{object_writer, CreateAccount(KVpid, <<"object.writer">>, [{<<"object.writer">>, riakacl_group:new_dt()}])},
			{admin, CreateAccount(KVpid, <<"admin">>, [{<<"admin">>, riakacl_group:new_dt()}])} | Config ],
	riakc_pool:unlock(KVpool, KVpid),
	AccountsConf.

-spec oneof(list()) -> integer().
oneof(L) ->
	lists:nth(rand:uniform(length(L)), L).

-spec vector(non_neg_integer(), list()) -> list().
vector(MaxSize, L) ->
	vector(0, MaxSize, L, []).

-spec vector(non_neg_integer(), non_neg_integer(), list(), list()) -> list().
vector(Size, MaxSize, L, Acc) when Size < MaxSize ->
	vector(Size +1, MaxSize, L, [oneof(L)|Acc]);
vector(_, _, _, Acc) ->
	Acc.

-spec alphanum_lowercase_chars() -> list().
alphanum_lowercase_chars() ->
	"0123456789abcdefghijklmnopqrstuvwxyz".

-spec alphanum_chars() -> list().
alphanum_chars() ->
	"0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ".
