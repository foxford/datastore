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

-module(datastore_streamh).

-include("datastore_log.hrl").

%% Stream handler callbacks
-export([
	init/3,
	data/4,
	info/3,
	terminate/3
]).

%% =============================================================================
%% Stream handler callbacks
%% =============================================================================

init(StreamId, Req, Opts) ->
	cowboy_stream_h:init(StreamId, Req, Opts).

data(StreamID, IsFin, Data, State) ->
	cowboy_stream_h:data(StreamID, IsFin, Data, State).

info(StreamId, {response, Status, Headers, _Body} =Response, State) ->
	?INFO_REPORT(datastore_http_log:format_response(StreamId, Status, Headers)),
	cowboy_stream_h:info(StreamId, Response, State);
info(StreamId, Message, State) ->
	cowboy_stream_h:info(StreamId, Message, State).

terminate(StreamId, Reason, State) ->
	cowboy_stream_h:terminate(StreamId, Reason, State).
