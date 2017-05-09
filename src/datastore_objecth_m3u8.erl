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

-module(datastore_objecth_m3u8).
-behaviour(datastore_objecth).

%% Media type handler callbacks
-export([
	handle_read/5,
	handle_read_stream/4,
	handle_read_body/4,
	cleanup_headers/1
]).

%% Definitions
-define(CONTENT_TYPE, <<"application/vnd.apple.mpegurl">>).
-define(IS_URISEP(C), (C =:= $\n) or (C =:= $\r) or (C =:= $\s) or (C =:= $\t) or (C =:= $")).

%% Types
-type parser_state() :: in | {out, binary()}.

-record(state, {
	t                :: binary(),
	ps = {out, <<>>} :: parser_state()
}).

%% =============================================================================
%% Media type handler callbacks
%% =============================================================================

-spec handle_read(Bucket, Key, Params, Status, GunHeaders) -> Result
	when
		Bucket        :: binary(),
		Key           :: binary(),
		Params        :: map(),
		Status        :: riaks2c_http:status(),
		GunHeaders    :: riaks2c_http:headers(),
		CowboyHeaders :: cowboy_stream:headers(),
		State         :: any(),
		Result        :: {await_body, State} | {stream, Status, CowboyHeaders, State}.
handle_read(_Bucket, _Key, #{access_token := Token}, 200 =Status, Headers) ->
	%% We're appending the access token to any URI that is presented
	%% in the body of response if it was passed as an query string argument
	%% (for user agents that can't use Authorization HTTP header).
	%% We only support responses with 200 status code because
	%% it's imposible to parse URIs for byte range requests.
	Hs0 = datastore_objecth:cleanup_content_headers(Headers),
	Hs1 = Hs0#{<<"content-type">> => ?CONTENT_TYPE},
	{stream, Status, Hs1, {?MODULE, #state{t = Token}}};
handle_read(_Bucket, _Key, _Params, Status, Headers) ->
	{stream, Status, cleanup_headers(Headers), ignore}.

-spec handle_read_stream(IsFin, Data, Stream, State) -> State
	when
		IsFin  :: riaks2c_http:fin(),
		Data   :: iodata(),
		Stream :: cowboy_req:req(),
		State  :: any().
handle_read_stream(IsFin, Data0, Stream, #state{t = Token, ps = Pstate0} =State) ->
	{Data1, Pstate1} = append_access_token(Data0, Token, Pstate0),
	cowboy_req:stream_body(Data1, IsFin, Stream),
	State#state{ps = Pstate1}.

-spec handle_read_body(Status, GunHeaders, Data, State) -> Result
	when
		Status        :: riaks2c_http:status(),
		GunHeaders    :: riaks2c_http:headers(),
		CowboyHeaders :: cowboy_stream:headers(),
		Data          :: iodata(),
		State         :: any(),
		Result        :: {Status, CowboyHeaders, Data | keep_body}.
handle_read_body(Status, Headers, _Body, _State) ->
	{Status, cleanup_headers(Headers), keep_body}.

-spec cleanup_headers(GunHeaders) -> CowboyHeaders
	when
		GunHeaders    :: riaks2c_http:headers(),
		CowboyHeaders :: cowboy_stream:headers().
cleanup_headers(Headers) ->
	datastore_objecth:cleanup_headers(Headers).

%% =============================================================================
%% Internal functions
%% =============================================================================

-spec append_access_token(binary(), binary(), parser_state()) -> {binary(), parser_state()}.
append_access_token(Data, Token, State) ->
	append_access_token(Data, Token, State, <<>>).

-spec append_access_token(binary(), binary(), parser_state(), binary()) -> {binary(), parser_state()}.
append_access_token(<<$h, Rest/bits>>, Token, {out, <<>>}, Acc)                       -> append_access_token(Rest, Token, {out, <<"h">>}, <<Acc/binary, $h>>);
append_access_token(<<$t, Rest/bits>>, Token, {out, <<"h">>}, Acc)                    -> append_access_token(Rest, Token, {out, <<"ht">>}, <<Acc/binary, $t>>);
append_access_token(<<$t, Rest/bits>>, Token, {out, <<"ht">>}, Acc)                   -> append_access_token(Rest, Token, {out, <<"htt">>}, <<Acc/binary, $t>>);
append_access_token(<<$p, Rest/bits>>, Token, {out, <<"htt">>}, Acc)                  -> append_access_token(Rest, Token, {out, <<"http">>}, <<Acc/binary, $p>>);
append_access_token(<<$s, Rest/bits>>, Token, {out, <<"http">>}, Acc)                 -> append_access_token(Rest, Token, {out, <<"https">>}, <<Acc/binary, $s>>);
append_access_token(<<$:, Rest/bits>>, Token, {out, <<"http">>}, Acc)                 -> append_access_token(Rest, Token, {out, <<"http:">>}, <<Acc/binary, $:>>);
append_access_token(<<$:, Rest/bits>>, Token, {out, <<"https">>}, Acc)                -> append_access_token(Rest, Token, {out, <<"https:">>}, <<Acc/binary, $:>>);
append_access_token(<<$/, Rest/bits>>, Token, {out, <<"http:">>}, Acc)                -> append_access_token(Rest, Token, {out, <<"http:/">>}, <<Acc/binary, $/>>);
append_access_token(<<$/, Rest/bits>>, Token, {out, <<"https:">>}, Acc)               -> append_access_token(Rest, Token, {out, <<"https:/">>}, <<Acc/binary, $/>>);
append_access_token(<<$/, Rest/bits>>, Token, {out, <<"http:/">>}, Acc)               -> append_access_token(Rest, Token, in, <<Acc/binary, $/>>);
append_access_token(<<$/, Rest/bits>>, Token, {out, <<"https:/">>}, Acc)              -> append_access_token(Rest, Token, in, <<Acc/binary, $/>>);
append_access_token(<<C, Rest/bits>>, Token, {out, Out}, Acc) when Out =/= <<"http">> -> append_access_token(Rest, Token, {out, <<>>}, <<Acc/binary, C>>);
append_access_token(<<C, Rest/bits>>, Token, in, Acc) when ?IS_URISEP(C)              -> append_access_token(Rest, Token, {out, <<>>}, <<Acc/binary, "?access_token=", Token/binary, C>>);
append_access_token(<<C, Rest/bits>>, Token, State, Acc)                              -> append_access_token(Rest, Token, State, <<Acc/binary, C>>);
append_access_token(<<>>, _Token, State, Acc)                                         -> {Acc, State}.

%% =============================================================================
%% Tests 
%% =============================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-define(TOKEN, <<"123">>).
-define(STATE, {out, <<>>}).

append_access_token_test_() ->
	Tests =
		[	{<<"xh">>, {<<"xh">>, {out, <<"h">>}}},
			{<<"h">>, {<<"h">>, {out, <<"h">>}}},
			{<<"hx">>, {<<"hx">>, {out, <<>>}}},
			{<<"http://">>, {<<"http://">>, in}},
			{<<"http://x">>, {<<"http://x">>, in}},
			{<<"#\shttp://x">>, {<<"#\shttp://x">>, in}},
			{<<"https://">>, {<<"https://">>, in}},
			{<<"https://x">>, {<<"https://x">>, in}},
			{<<"#\shttps://x">>, {<<"#\shttps://x">>, in}},
			{<<"http://example.org\n">>, {<<"http://example.org?access_token=123\n">>, {out, <<>>}}},
			{<<"https://example.org\n">>, {<<"https://example.org?access_token=123\n">>, {out, <<>>}}},
			{<<"#\shttp://example.org\s">>, {<<"#\shttp://example.org?access_token=123\s">>, {out, <<>>}}},
			{<<"#\shttps://example.org\s">>, {<<"#\shttps://example.org?access_token=123\s">>, {out, <<>>}}} ],

	[{lists:flatten(io_lib:format("~p", [Input])),
		?_assertEqual(Output, append_access_token(Input, ?TOKEN, ?STATE))} || {Input, Output} <- Tests].

append_access_token_2parts_test_() ->
	Tests =
		[	{<<"https://example.org\n">>, <<>>, <<"https://example.org?access_token=123\n">>},
			{<<"h">>, <<"ttps://example.org\n">>, <<"https://example.org?access_token=123\n">>},
			{<<"https:/">>, <<"/example.org\n">>, <<"https://example.org?access_token=123\n">>},
			{<<"https://">>, <<"example.org\n">>, <<"https://example.org?access_token=123\n">>},
			{<<>>, <<"https://example.org\n">>, <<"https://example.org?access_token=123\n">>} ],

	[{lists:flatten(io_lib:format("~p, ~p", [Input0, Input1])),
		begin
			{Acc0, State} = append_access_token(Input0, ?TOKEN, ?STATE),
			{Acc1, _} = append_access_token(Input1, ?TOKEN, State),
			?_assertEqual(Output, <<Acc0/binary, Acc1/binary>>)
		end} || {Input0, Input1, Output} <- Tests].

-endif.
