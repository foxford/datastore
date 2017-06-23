-module(datastore_constraint).

%% API
-export([
	binary/2,
	int/2
]).

%% Types
-type constraint() :: fun((iodata()) -> {true, any()} | true | false).

%% =============================================================================
%% API
%% =============================================================================

-spec binary(non_neg_integer(), non_neg_integer()) -> constraint().
binary(Min, Max) ->
	fun(Val) ->
		try
			Size = iolist_size(Val),
			true = Size =< Max,
			true = Size >= Min,
			true
		catch _:_ -> false end
	end.

-spec int(non_neg_integer(), non_neg_integer()) -> constraint().
int(Min, Max) ->
	fun(Val) ->
		try
			Num = binary_to_integer(Val),
			true = Num =< Max,
			true = Num >= Min,
			{true, Num}
		catch _:_ -> false end
	end.
