-module(advent1).
-compile(export_all).

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
  [R1|_] = [A*B || A <- L, B <- L, A /= B, A+B == 2020],
  [R2|_] = [A*B*C || A <- L, B <- L, C <- L, A /= B, A+B+C == 2020],
  {R1, R2}.