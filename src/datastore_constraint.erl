-module(datastore_constraint).

%% API
-export([
	binary/2,
	int/2
]).

%% Types
-type constraint() :: fun((forward | reverse | format_error, iodata()) -> {ok, any()} | {error, any()} | iodata()).

%% =============================================================================
%% API
%% =============================================================================

-spec binary(non_neg_integer(), non_neg_integer()) -> constraint().
binary(Min, Max) ->
	fun
		(forward, Val) ->
			try
				Size = iolist_size(Val),
				true = Size =< Max,
				true = Size >= Min,
				{ok, Val}
			catch _:_ -> {error, {invalid_binary, Val}} end;
		(reverse, Val) ->
			{ok, Val};
		(format_error, {invalid_binary, Val}) ->
			<<"The value ", Val/binary,
				"should be a binary whose length is (", (integer_to_binary(Min))/binary,
				$,, (integer_to_binary(Max))/binary, ").">>
	end.

-spec int(non_neg_integer(), non_neg_integer()) -> constraint().
int(Min, Max) ->
	fun
		(forward, Val) ->
			try
				Num = binary_to_integer(Val),
				true = Num =< Max,
				true = Num >= Min,
				{ok, Num}
			catch _:_ -> {error, {invalid_integer, Val}} end;
		(reverse, Val) ->
			try {ok, integer_to_binary(Val)}
			catch _:_ -> {error, {invalid_integer, Val}} end;
		(format_error, {invalid_integer, Val}) ->
			<<"The value ", (integer_to_binary(Val))/binary,
				"should be an integer whose range is (", (integer_to_binary(Min))/binary,
				$,, (integer_to_binary(Max))/binary, ").">>
	end.
