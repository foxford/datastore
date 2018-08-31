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

-module(datastore_httph_object).

-include("datastore_log.hrl").

%% HTTP handler callbacks
-export([
	init/2
]).

%% Types
-record(state, {
	rdesc           :: map(),
	authconf        :: map(),
	bucket          :: iodata(),
	set             :: iodata() | undefined,
	key             :: iodata(),
	params    = #{} :: map(),
	s2reqopts = #{} :: riaks2c_http:request_options(),
	authm     = #{} :: map()
}).

%% =============================================================================
%% HTTP handler callbacks
%% =============================================================================

init(Req, Opts) ->
	#{method := Method} = Req,
	#{authentication := AuthConf, resources := Rdesc} = Opts,
	State =
		#state{
			rdesc = Rdesc,
			authconf = AuthConf,
			key = cowboy_req:binding(key, Req),
			set = cowboy_req:binding(set, Req),
			bucket = cowboy_req:binding(bucket, Req)},

	handle_request(Method, Req, State).

%% =============================================================================
%% Internal functions
%% =============================================================================

% handle_request(<<"HEAD">>, Req, State)     -> handle_headers(Req, State);
% handle_request(<<"GET">>, Req, State)      -> handle_headers(Req, State);
% handle_request(<<"PUT">>, Req, State)      -> handle_headers(Req, State);
% handle_request(<<"DELETE">>, Req, State)   -> handle_headers(Req, State);

handle_request(<<"HEAD">>, Req, State)     -> handle_params(Req, State);
handle_request(<<"GET">>, Req, State)      -> handle_params(Req, State);
handle_request(<<"OPTIONS">>, Req0, State) ->
	% Hs = cow_http_hd:access_control_allow_headers(allow_headers(cowboy_req:header(<<"access-control-request-method">>, Req0))),
	% Req1 = cowboy_req:set_resp_header(<<"access-control-allow-methods">>, <<"HEAD, GET, PUT, DELETE">>, Req0),
	% Req1 = cowboy_req:set_resp_header(<<"access-control-allow-methods">>, <<"HEAD, GET, PUT">>, Req0),
	Req1 = cowboy_req:set_resp_header(<<"access-control-allow-methods">>, <<"HEAD, GET">>, Req0),
	Req2 = cowboy_req:set_resp_header(<<"access-control-allow-headers">>, <<"authorization, cache-control, if-match, if-modified-since, if-none-match, if-unmodified-since, range">>, Req1),
	Req3 = cowboy_req:set_resp_header(<<"access-control-allow-credentials">>, <<"true">>, Req2),
	{ok, cowboy_req:reply(200, Req3), State};
handle_request(_, Req, State) ->
	{ok, cowboy_req:reply(405, Req), State}.

% handle_headers(#{method := Method, headers := Hs} =Req, State) ->
% 	try with_headers(allow_riaks2_headers(Method), Hs) of
% 		S2headers ->
% 			handle_params(Req, State#state{s2reqopts = #{headers => S2headers}})
% 	catch
% 		T:R ->
% 			?ERROR_REPORT([{http_headers, Hs} | datastore_http_log:format_request(Req)], T, R),
% 			{ok, cowboy_req:reply(400, Req), State}
% 	end.

handle_params(#{method := Method} =Req, State) ->
	try parse_params(Method, Req) of
		Params ->
			handle_authentication(Req, State#state{params = Params})
	catch
		T:R ->
			?ERROR_REPORT(datastore_http_log:format_request(Req), T, R),
			{ok, cowboy_req:reply(400, Req), State}
	end.

handle_authentication(Req, #state{authconf = AuthConf, params = Params} =State) ->
	try case Params of
		#{access_token := Token} -> datastore:decode_access_token(Token, AuthConf);
		_                        -> datastore_http:decode_access_token(Req, AuthConf)
	end of TokenPayload ->
		%% ?INFO_REPORT([{access_token, TokenPayload} | datastore_http_log:format_request(Req)]),
		handle_authorization(Req, State#state{authm = TokenPayload})
	catch T:R ->
		?ERROR_REPORT(datastore_http_log:format_unauthenticated_request(Req), T, R),
		{ok, cowboy_req:reply(401, Req), State}
	end.

handle_authorization(#{method := <<"HEAD">>} =Req, State)   -> handle_read_authorization(Req, State);
handle_authorization(#{method := <<"GET">>} =Req, State)    -> handle_read_authorization(Req, State).
% handle_authorization(#{method := <<"PUT">>} =Req, State)    -> handle_write_authorization(Req, State).
% handle_authorization(#{method := <<"DELETE">>} =Req, State) -> handle_write_authorization(Req, State).

handle_read_authorization(Req, State) ->
	handle_redirect(Req, State).

handle_redirect(#{method := Method} = Req, #state{key = Key, set = Set, bucket = Bucket, s2reqopts = S2reqopts, rdesc = Rdesc} =State) ->
	#{host := Host, port := Port, schema := Schema} = datastore:cdn_redirect_options(Bucket, Set, Rdesc),
	#{object := #{options := S2opts}} = Rdesc,

	Expires = datastore:unix_time() + datastore:expires_in(),
	Path = riaks2c_object:signed_uri(Bucket, datastore:object_key(Set, Key), Method, Expires, S2reqopts, S2opts),
	Location = iolist_to_binary([Schema, Host, <<$:>>, integer_to_binary(Port), Path]),

	{ok, cowboy_req:reply(307, #{<<"location">> => Location}, Req), State}.

% handle_write_authorization(Req, #state{rdesc = Rdesc, bucket = Bucket, authm = AuthM} =State) ->
% 	try datastore:authorize(Bucket, AuthM, Rdesc) of
% 		{ok, #{write := true}} -> handle_redirect(Req, State);
% 		_                      -> {ok, cowboy_req:reply(403, Req), State}
% 	catch
% 		T:R ->
% 			?ERROR_REPORT(datastore_http_log:format_request(Req), T, R),
% 			{ok, cowboy_req:reply(422, Req), State}
% 	end.

% handle_read_authorization(Req, #state{rdesc = Rdesc, bucket = Bucket, key = Key, authm = AuthM} =State) ->
% 	try datastore:authorize(datastore_acl:object_key(Bucket, Key), AuthM, Rdesc) of
% 		{ok, #{read := true}}      -> handle_read(Req, State);
% 		{ok, _}                    -> handle_read_authorization_maybe_notfound(Req, State);
% 		{error, bad_aclobject_key} -> handle_read_authorization_maybe_notfound(Req, State);
% 		_                          -> {ok, cowboy_req:reply(403, Req), State}
% 	catch
% 		T:R ->
% 			?ERROR_REPORT(datastore_http_log:format_request(Req), T, R),
% 			{ok, cowboy_req:reply(422, Req), State}
% 	end.

% %% If subject doesn't have access to non-existent object
% %%   - and doesn't have read access to object's bucket, 403 status code is returned
% %%   - but have read access to object's bucket, 404 status code is returned
% handle_read_authorization_maybe_notfound(Req, #state{rdesc = Rdesc, bucket = Bucket, authm = AuthM} =State) ->
% 	try datastore:authorize(Bucket, AuthM, Rdesc) of
% 		{ok, #{read := true}} -> handle_maybe_notfound(Req, State);
% 		_                     -> {ok, cowboy_req:reply(403, Req), State}
% 	catch T:R ->
% 		?ERROR_REPORT(datastore_http_log:format_request(Req), T, R),
% 		{stop, cowboy_req:reply(422, Req), State}
% 	end.

% handle_maybe_notfound(Req0, #state{rdesc = Rdesc, bucket = Bucket, key = Key, s2reqopts = S2reqopts} =State) ->
% 	#{object := #{pool := S2pool, options := S2opts, lock_timeout := LockTimeout, read_timeout := ReadTimeout}} = Rdesc,
% 	try gunc_pool:lock(S2pool, LockTimeout) of
% 		S2pid ->
% 			Ref = riaks2c_object:head(S2pid, Bucket, Key, S2reqopts, S2opts),
% 			Req1 =
% 				try riaks2c_object:expect_head(S2pid, Ref, ReadTimeout) of
% 					{Status, _Headers} when Status >= 200, Status < 300 -> cowboy_req:reply(403, Req0);
% 					_                                                   -> cowboy_req:reply(404, Req0)
% 				catch T:R ->
% 					?ERROR_REPORT(datastore_http_log:format_request(Req0), T, R),
% 					riaks2c_http:cancel(S2pid, Ref),
% 					cowboy_req:reply(422, Req0)
% 				end,
% 			gunc_pool:unlock(S2pool, S2pid),
% 			{ok, Req1, State}
% 	catch T:R ->
% 		?ERROR_REPORT(datastore_http_log:format_request(Req0), T, R),
% 		{ok, cowboy_req:reply(503, Req0), State}
% 	end.

% handle_write_authorization(Req, #state{rdesc = Rdesc, bucket = Bucket, authm = AuthM} =State) ->
% 	try datastore:authorize(Bucket, AuthM, Rdesc) of
% 		{ok, #{write := true}} -> handle_write(Req, State);
% 		_                      -> {ok, cowboy_req:reply(403, Req), State}
% 	catch
% 		T:R ->
% 			?ERROR_REPORT(datastore_http_log:format_request(Req), T, R),
% 			{ok, cowboy_req:reply(422, Req), State}
% 	end.

% handle_read(#{method := Method} =Req0, #state{rdesc = Rdesc, key = Key, params = Params, bucket = Bucket, s2reqopts = S2reqopts} =State) ->
% 	#{object := #{pool := S2pool, options := S2opts, lock_timeout := LockTimeout, read_timeout := ReadTimeout, handler := Hmod}} = Rdesc,

% 	try gunc_pool:lock(S2pool, LockTimeout) of
% 		S2pid ->
% 			Ref =
% 				case Method of
% 					<<"HEAD">> -> riaks2c_object:head(S2pid, Bucket, Key, S2reqopts, S2opts);
% 					<<"GET">>  -> riaks2c_object:get(S2pid, Bucket, Key, S2reqopts, S2opts)
% 				end,

% 			Req1 =
% 				try handle_read_stream(Hmod, Bucket, Key, Params, S2pid, Ref, ReadTimeout, Req0)
% 				catch T:R ->
% 					?ERROR_REPORT(datastore_http_log:format_request(Req0), T, R),
% 					riaks2c_http:cancel(S2pid, Ref),
% 					cowboy_req:reply(422, Req0)
% 				end,
% 			gunc_pool:unlock(S2pool, S2pid),
% 			{ok, Req1, State}
% 	catch
% 		T:R ->
% 			?ERROR_REPORT(datastore_http_log:format_request(Req0), T, R),
% 			{ok, cowboy_req:reply(503, Req0), State}
% 	end.

% handle_write(#{method := <<"PUT">>} =Req, State)    -> handle_update(Req, State);
% handle_write(#{method := <<"DELETE">>} =Req, State) -> handle_delete(Req, State).

% handle_delete(Req0, #state{rdesc = Rdesc, key = Key, bucket = Bucket, s2reqopts = S2reqopts} =State) ->
% 	#{object := #{pool := S2pool, options := S2opts, lock_timeout := LockTimeout, read_timeout := ReadTimeout}} = Rdesc,

% 	%% Removing object
% 	try gunc_pool:lock(S2pool, LockTimeout) of
% 		S2pid ->
% 			Ref = riaks2c_object:remove(S2pid, Bucket, Key, S2reqopts, S2opts),
% 			MaybeOk =
% 				try riaks2c_object:await_remove(S2pid, Ref, ReadTimeout)
% 				catch T:R ->
% 					?ERROR_REPORT(datastore_http_log:format_request(Req0), T, R),
% 					riaks2c_http:cancel(S2pid, Ref),
% 					{error, uncertain_operation_state}
% 				end,
% 			gunc_pool:unlock(S2pool, S2pid),

% 			%% Removing ACL (if object has been successfully removed)
% 			Req1 =
% 				case MaybeOk of
% 					ok                                 -> handle_delete_acl(204, Req0, State);
% 					{error, {bad_bucket, _Bucket}}     -> handle_delete_acl(404, Req0, State);
% 					{error, {bad_key, _Bucket, _Key}}  -> handle_delete_acl(404, Req0, State);
% 					{error, uncertain_operation_state} -> cowboy_req:reply(504, Req0);
% 					{error, Reason}                    -> exit({bad_riaks2_response, Reason})
% 				end,
% 			{ok, Req1, State}
% 	catch
% 		T:R ->
% 			?ERROR_REPORT(datastore_http_log:format_request(Req0), T, R),
% 			{ok, cowboy_req:reply(503, Req0), State}
% 	end.

% handle_delete_acl(SuccessStatus, Req, #state{rdesc = Rdesc, key = Key, bucket = Bucket, params = Params}) ->
% 	#{object_aclobject := #{pool := KVpool, bucket := AclObjBucket}} = Rdesc,
% 	case maps:get(keepacl, Params) of
% 		true -> ignore;
% 		_ ->
% 			KVpid = riakc_pool:lock(KVpool),
% 			try riakacl_entry:remove(KVpid, AclObjBucket, datastore_acl:object_key(Bucket, Key))
% 			catch T:R -> ?ERROR_REPORT(datastore_http_log:format_request(Req), T, R) end,
% 			riakc_pool:unlock(KVpool, KVpid)
% 	end,
% 	cowboy_req:reply(SuccessStatus, Req).

% handle_update(Req0, #state{rdesc = Rdesc, key = Key, bucket = Bucket, s2reqopts = S2reqopts} =State) ->
% 	#{object := #{pool := S2pool, options := S2opts, lock_timeout := LockTimeout, write_timeout := WriteTimeout}} = Rdesc,

% 	%% Uploading object
% 	try gunc_pool:lock(S2pool, LockTimeout) of
% 		S2pid ->
% 			Ref = riaks2c_object:put(S2pid, Bucket, Key, <<>>, S2reqopts, S2opts),
% 			try upstream_body(S2pid, Ref, WriteTimeout, Req0) of
% 				Req1 ->
% 					MaybeOk = 
% 						try riaks2c_object:await_put(S2pid, Ref, WriteTimeout)
% 						catch T:R ->
% 							?ERROR_REPORT(datastore_http_log:format_request(Req1), T, R),
% 							riaks2c_http:cancel(S2pid, Ref),
% 							{error, uncertain_operation_state}
% 						end,
% 					gunc_pool:unlock(S2pool, S2pid),

% 					Req2 =
% 						case MaybeOk of
% 							ok                                         -> handle_update_acl(204, Req1, State);
% 							{error, {bad_bucket, _Bucket}}             -> cowboy_req:reply(404, Req1);
% 							{error, {bad_precondition, _Bucket, _Key}} -> cowboy_req:reply(412, Req1);
% 							{error, uncertain_operation_state}         -> cowboy_req:reply(504, Req1);
% 							{error, Reason}                            -> exit({bad_riaks2_response, Reason})
% 						end,
% 					{ok, Req2, State}
% 			catch
% 				T:R ->
% 					?ERROR_REPORT(datastore_http_log:format_request(Req0), T, R),
% 					riaks2c_http:cancel(S2pid, Ref),
% 					gunc_pool:unlock(S2pool, S2pid),
% 					{ok, cowboy_req:reply(408, Req0), State}
% 			end
% 	catch
% 		T:R ->
% 			?ERROR_REPORT(datastore_http_log:format_request(Req0), T, R),
% 			{ok, cowboy_req:reply(503, Req0), State}
% 	end.

% handle_update_acl(SuccessStatus, Req, #state{rdesc = Rdesc, key = Key, bucket = Bucket, params = Params}) ->
% 	#{object_aclobject := #{pool := KVpool, bucket := AclObjBucket}} = Rdesc,

% 	case maps:get(aclgroups, Params) of
% 		[_|_] =Groups ->
% 			KVpid = riakc_pool:lock(KVpool),
% 			Req2 =
% 				try riakacl:put_object_acl(KVpid, AclObjBucket, datastore_acl:object_key(Bucket, Key), Groups) of
% 					_ ->
% 						%% ACL groups have been successfully updated
% 						cowboy_req:reply(SuccessStatus, Req)
% 				catch
% 					T:R ->
% 						%% ACL groups haven't been updated 
% 						?ERROR_REPORT(datastore_http_log:format_request(Req), T, R),
% 						cowboy_req:reply(422, Req)
% 				end,
% 			riakc_pool:unlock(KVpool, KVpid),
% 			Req2;
% 		_ ->
% 			%% No ACL groups to update
% 			cowboy_req:reply(SuccessStatus, Req)
% 	end.

% -spec upstream_body(pid(), reference(), non_neg_integer(), Req) -> Req when Req :: cowboy_req:req().
% upstream_body(Pid, Ref, WriteTimeout, Req0) ->
% 	case cowboy_req:read_body(Req0, #{timeout => WriteTimeout}) of
% 		{ok, Data, Req1} ->
% 			gun:data(Pid, Ref, fin, Data),
% 			Req1;
% 		{more, Data, Req1} ->
% 			gun:data(Pid, Ref, nofin, Data),
% 			upstream_body(Pid, Ref, WriteTimeout, Req1)
% 	end.

% -spec handle_read_stream(module(), binary(), binary(), map(), pid(), reference(), non_neg_integer(), Req) -> Req when Req :: cowboy_req:req().
% handle_read_stream(Hmod, Bucket, Key, Params, Pid, Ref, Timeout, #{method := Method} =Req) ->
% 	Mref = monitor(process, Pid),
% 	case riaks2c_object:expect_head(Pid, Ref, Timeout, Mref) of
% 		{Status, Headers} when Status >= 200, Status < 300 ->
% 			case Hmod:handle_read(Bucket, Key, Params, Status, Headers) of
% 				{stream, Hst, Hhs, Hstate} ->
% 					case Method of
% 						<<"HEAD">> -> cowboy_req:reply(204, Hhs, Req);
% 						<<"GET">> ->
% 							Stream = cowboy_req:stream_reply(Hst, Hhs, Req),
% 							riaks2c_http:fold_body(
% 								Pid, Ref, Timeout, Mref, Hstate,
% 								fun(IsFin, Data, Acc) ->
% 									Hmod:handle_read_stream(IsFin, Data, Stream, Acc)
% 								end),
% 							Stream
% 					end;
% 				{await_body, Hstate} ->
% 					case Method of
% 						<<"HEAD">> -> cowboy_req:reply(204, Hmod:cleanup_headers(Headers), Req);
% 						<<"GET">> ->
% 							Body = riaks2c_object:expect_body(Pid, Ref, Timeout, Mref),
% 							case Hmod:handle_read_body(Status, Headers, Body, Hstate) of
% 								{Hst, Hhs, keep_body} -> cowboy_req:reply(Hst, Hhs, Body, Req);
% 								{Hst, Hhs, Hbody}     -> cowboy_req:reply(Hst, Hhs, Hbody, Req)
% 							end
% 					end
% 			end;
% 		{Status, Headers} when (Status =:= 304) or (Status =:= 404) or (Status =:= 412) or (Status =:= 416) ->
% 			demonitor(Mref, [flush]),
% 			riaks2c_http:cancel(Pid, Ref),
% 			riaks2c_http:flush(Ref),
% 			cowboy_req:reply(Status, Hmod:cleanup_headers(Headers), Req);
% 		{Status, _Headers} ->
% 			demonitor(Mref, [flush]),
% 			riaks2c_http:cancel(Pid, Ref),
% 			riaks2c_http:flush(Ref),
% 			exit({bad_riaks2_response_status, Status})
% 	end.

%-spec allow_headers(binary()) -> [binary()].
% allow_headers(<<"PUT">> =Method) ->
% 	[	<<"authorization">>,
% 		<<"x-datastore-acl">>
% 		| [H || {H, _} <- allow_riaks2_headers(Method)]];
%allow_headers(Method) ->
%	[	<<"authorization">>
%		| [H || {H, _} <- allow_riaks2_headers(Method)]].

% -spec allow_riaks2_headers(binary()) -> [Validator] when Validator :: {binary(), fun((iodata()) -> any())}.
% allow_riaks2_headers(Method) when (Method =:= <<"GET">>) or (Method =:= <<"HEAD">>) ->
% 	[	{<<"cache-control">>, fun cow_http_hd:parse_cache_control/1},
% 		{<<"if-match">>, fun cow_http_hd:parse_if_match/1},
% 		{<<"if-modified-since">>, fun cow_http_hd:parse_if_modified_since/1},
% 		{<<"if-none-match">>, fun cow_http_hd:parse_if_none_match/1},
% 		{<<"if-unmodified-since">>, fun cow_http_hd:parse_if_unmodified_since/1},
% 		{<<"range">>, fun cow_http_hd:parse_range/1} ].
% allow_riaks2_headers(<<"PUT">>) ->
% 	[	%% We can't support 'Expect: 100-continue' HTTP header,
% 		%% because Cowboy doesn't support 1xx series of HTTP response codes.
% 		%% https://github.com/ninenines/cowboy/issues/1049
% 		%% {<<"expect">>, fun cow_http_hd:expect/1},
% 		{<<"expires">>, fun cow_http_hd:parse_expires/1},
% 		{<<"cache-control">>, fun cow_http_hd:parse_cache_control/1},
% 		{<<"content-disposition">>, fun(_) -> ok end},
% 		{<<"content-encoding">>, fun cow_http_hd:parse_content_encoding/1},
% 		{<<"content-length">>, fun cow_http_hd:parse_content_length/1},
% 		{<<"content-md5">>, fun(_) -> ok end},
% 		{<<"content-type">>, fun cow_http_hd:parse_content_type/1},
% 		{<<"if-match">>, fun cow_http_hd:parse_if_match/1},
% 		{<<"if-none-match">>, fun cow_http_hd:parse_if_none_match/1},
% 		{<<"if-unmodified-since">>, fun cow_http_hd:parse_if_unmodified_since/1} ];
% allow_riaks2_headers(<<"DELETE">>) ->
% 	[].

-spec parse_params(binary(), cowboy_req:req()) -> map().
parse_params(_Method, Req) -> parse_read_params(cowboy_req:parse_qs(Req), #{}).
%parse_params(<<"HEAD">>, Req) -> parse_read_params(cowboy_req:parse_qs(Req), #{});
%parse_params(<<"GET">>, Req)  -> parse_read_params(cowboy_req:parse_qs(Req), #{});
%parse_params(_Method, _Req)   -> #{}.
% parse_params(<<"PUT">>, Req) ->
% 	true = cowboy_req:has_body(Req),
% 	true = undefined =/= cowboy_req:header(<<"content-length">>, Req),
% 	#{aclgroups => parse_aclheader(Req)};
% parse_params(<<"DELETE">>, Req) ->
% 	#{keepacl => parse_keepaclheader(Req)}.

-spec parse_read_params([{binary(), binary() | true}], map()) -> map().
parse_read_params([{<<"access_token">>, Val}|T], M) -> parse_read_params(T, M#{access_token => Val});
parse_read_params([_|T], M)                         -> parse_read_params(T, M);
parse_read_params([], M)                            -> M.

% -spec with_headers([Validator], map()) -> Headers
% 	when
% 		Validator :: {binary(), fun((iodata()) -> any())},
% 		Headers :: [{binary(), iodata()}].
% with_headers(Keys, Headers) ->
% 	with_headers(Keys, Headers, []).

% -spec with_headers([Validator], map(), Headers) -> Headers
% 	when
% 		Validator :: {binary(), fun((iodata()) -> any())},
% 		Headers :: [{binary(), iodata()}].
% with_headers([{Key, Validate}|T], Headers, Acc) ->
% 	case maps:find(Key, Headers) of
% 		{ok, Val} ->
% 			try Validate(Val) of
% 				_ -> with_headers(T, Headers, [{Key, Val}|Acc])
% 			catch _:_ -> error({bad_http_header, Val}) end;
% 		_ ->
% 			with_headers(T, Headers, Acc)
% 	end;
% with_headers([], _Headers, Acc) ->
% 	Acc.

% -spec parse_aclheader(cowboy_req:req()) -> [{binary(), riakacl_group:group()}].
% parse_aclheader(Req) ->
% 	case cowboy_req:header(<<"x-datastore-acl">>, Req) of
% 		undefined -> [];
% 		Val       -> datastore_acl:parse_resources(jsx:decode(Val))
% 	end.

% -spec parse_keepaclheader(cowboy_req:req()) -> boolean().
% parse_keepaclheader(Req) ->
% 	case cowboy_req:header(<<"x-datastore-keep-acl">>, Req) of
% 		undefined  -> false;
% 		<<"true">> -> true;
% 		Val        -> error({bad_keepaclheader, Val})
% 	end.