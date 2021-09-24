-module(split_list).
-export([divide/2]).


divide(L, N) when is_integer(N), N > 1 -> divide(N, 0, L, []).

divide(_, _, [], Acc) -> [lists:reverse(Acc)];
divide(N, N, L, Acc) ->
	[lists:reverse(Acc) | divide(N, 0, L, [])]; % finished a group (end of loop)
divide(N, X, [H|T], Acc) ->
	divide(N, X+1, T, [H|Acc]). % add new element to group
