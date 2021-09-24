-module(advent9).
-export([start/0]).

-define(PREAMBLE_SIZE, 25).


is_weakness(L, N) -> [] == [A+B || A <- L, B <- L, A /= B, A+B == N].
process_list([_|T]=L) when length(L) >= ?PREAMBLE_SIZE+1 ->
	L2 = [lists:nth(I, L) || I <- lists:seq(1,?PREAMBLE_SIZE)],
	N = lists:nth(?PREAMBLE_SIZE+1,L),
	case is_weakness(L2, N) of
		true -> N;
		_ -> process_list(T)
	end;
process_list(_) -> none.

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


start() -> 
	L = readlines("input.txt"),
	process_list(L).
