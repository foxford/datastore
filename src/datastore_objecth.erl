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

-module(datastore_objecth).

%% Media type handler callbacks
-export([
	handle_read/5,
	handle_read_stream/4,
	handle_read_body/4
]).

%% API
-export([
	cleanup_headers/1
]).

%% Callbacks
-callback handle_read(Bucket, Key, Params, Status, GunHeaders) -> Result
	when
		Bucket        :: binary(),
		Key           :: binary(),
		Params        :: map(),
		Status        :: riaks2c_http:status(),
		GunHeaders    :: riaks2c_http:headers(),
		CowboyHeaders :: cowboy_stream:headers(),
		State         :: any(),
		Result        :: {await_body, State} | {stream, Status, CowboyHeaders, State}.

-callback handle_read_stream(Data, Fin, Stream, State) -> State
	when
		Data   :: iodata(),
		Fin    :: riaks2c_http:fin(),
		Stream :: cowboy_req:req(),
		State  :: any().

-callback handle_read_body(Status, GunHeaders, Data, State) -> Result
	when
		Status        :: riaks2c_http:status(),
		GunHeaders    :: riaks2c_http:headers(),
		CowboyHeaders :: cowboy_stream:headers(),
		Data          :: iodata(),
		State         :: any(),
		Result        :: {Status, CowboyHeaders, Data | keep_body}.

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
handle_read(_Bucket, _Key, _Params, Status, Headers) ->
	{stream, Status, cleanup_headers(Headers), ignore}.

-spec handle_read_stream(Data, Fin, Stream, State) -> State
	when
		Data   :: iodata(),
		Fin    :: riaks2c_http:fin(),
		Stream :: cowboy_req:req(),
		State  :: any().
handle_read_stream(Data, IsFin, Stream, State) ->
	cowboy_req:stream_body(Data, IsFin, Stream),
	State.

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

%% =============================================================================
%% API
%% =============================================================================

-spec cleanup_headers(riaks2c_http:headers()) -> cowboy_stream:headers().
cleanup_headers(Headers) ->
	cleanup_headers(Headers, #{}).

-spec cleanup_headers(riaks2c_http:headers(), cowboy_stream:headers()) -> cowboy_stream:headers().
cleanup_headers([{<<"server">>, _}|T], Acc)        -> cleanup_headers(T, Acc);
cleanup_headers([{<<"x-", _/bits>>, _}|T], Acc)    -> cleanup_headers(T, Acc);
cleanup_headers([{Key, Val}|T], Acc)               -> cleanup_headers(T, Acc#{Key => Val});
cleanup_headers([], Acc)                           -> Acc.
