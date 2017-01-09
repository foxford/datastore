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

-module(datastore_http_log).

%% API
-export([
	format_request/1,
	format_unauthenticated_request/1,
	format_response/3
]).

%% Types
-type kvlist() :: [{atom(), any()}].

%% =============================================================================
%% API
%% =============================================================================

-spec format_request(cowboy_req:req()) -> kvlist().
format_request(Req) ->
	#{streamid := StreamId,
		method := Method,
		version := Version,
		headers := Headers,
		peer := {Addr, Port}} = Req,
	Acc =
		[	{http_streamid, StreamId},
			{http_uri, iolist_to_binary(cowboy_req:uri(Req))},
			{http_method, Method},
			{http_version, Version},
			{http_peer, <<(list_to_binary(inet:ntoa(Addr)))/binary, $:, (integer_to_binary(Port))/binary>>} ],
	add_optional_map_property(http_user_agent, <<"user-agent">>, Headers, Acc).

-spec format_unauthenticated_request(cowboy_req:req()) -> kvlist().
format_unauthenticated_request(#{headers := Headers} =Req) ->
	add_optional_map_property(http_authorization_header, <<"authorization">>, Headers, format_request(Req)).

-spec format_response(integer(), integer(), map()) -> kvlist().
format_response(StreamId, Status, Headers) ->
	[	{http_streamid, StreamId},
		{http_headers, Headers},
		{http_status_code, Status} ].

%% =============================================================================
%% Internal function
%% =============================================================================

-spec add_optional_map_property(atom(), any(), map(), kvlist()) -> kvlist().
add_optional_map_property(Label, Key, M, Acc) ->
	case maps:find(Key, M) of
		{ok, Val} -> [{Label, Val}|Acc];
		_         -> Acc
	end.
