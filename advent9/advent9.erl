-module(advent9).
-export([run/0, run_with_timer/0]).
-import(split_list, [divide/2]).
-define(PREAMBLE_SIZE, 25).

% This link https://stackoverflow.com/questions/12534898/splitting-a-list-in-equal-sized-chunks-in-erlang/12538415 can help in the second part
% https://stackoverflow.com/questions/31395608/how-to-split-a-list-of-strings-into-given-number-of-lists-in-erlang

is_weakness(L, N) -> [] == [A+B || A <- L, B <- L, A /= B, A+B == N].

process_list([_|T]=L, Index) when length(L) >= ?PREAMBLE_SIZE+1 ->
	L2 = [lists:nth(I, L) || I <- lists:seq(1,?PREAMBLE_SIZE)],
	N = lists:nth(?PREAMBLE_SIZE+1,L),
	case is_weakness(L2, N) of
		true -> {Index+4, N};
		_ -> process_list(T, Index+1)
	end;
process_list(_, _) -> none.

readlines(FileName) ->
	{ok, Device} = file:open(FileName, [read]),
	try get_all_lines(Device, [])
		after file:close(Device)
	end.

get_all_lines(Device, Lines) ->
	case io:get_line(Device, "") of
		eof -> lists:reverse(Lines);
		Line -> 
			N = re:replace(Line, "\\n", "", [global, {return, list}]),
			get_all_lines(Device, [list_to_integer(N) | Lines])
	end.


run() -> 
	L = readlines("input.txt"),
	{Index, N} = process_list(L, 1),
	Set = find_contiguous_set(N, L, Index),
	Sum = get_min(Set) + get_max(Set),
	{ok, {weak_number, N}, {set, Set}, {encryption_weakness, Sum}}.

run_with_timer() -> 
	{Micro, Result} = timer:tc(advent9, run, []),
	io:format("Finished in ~p milliseconds~n~n", [Micro/1000]),
	Result.


get_max([H|T]) -> get_max(T, H);
get_max([]) -> {error, empty_list}.
get_max([H|T], Cur) when Cur < H -> get_max(T, H);
get_max([H|T], Cur) when Cur >= H -> get_max(T, Cur);
get_max([], Cur) -> Cur.

get_min([H|T]) -> get_min(T, H).
get_min([H|T], Cur) when Cur > H -> get_min(T, H);
get_min([H|T], Cur) when Cur =< H -> get_min(T, Cur);
get_min([], Cur) -> Cur.


% Find the contiguous set adding last number with previuos up to the result (start from the last number before the weak number)
find_contiguous_set(N, List, Index) when is_integer(N), is_integer(Index), is_list(List), Index > 0 -> 
	SubList = [lists:nth(I, List) || I <- lists:seq(1, Index)],
	find_set(N, lists:reverse(SubList), [], 0).

find_set(N, [H|T], Acc, Sum) when Sum < N -> find_set(N, T, [H | Acc], H+Sum);
find_set(N, _, Acc, N) -> Acc;
find_set(N, List, Acc, _) -> 
	[_|T] = lists:reverse(Acc) ++ List,
	find_set(N, T, [], 0).