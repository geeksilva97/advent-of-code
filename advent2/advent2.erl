-module(advent2).
-compile(export_all).

-define(REGEX, "(\\d+)-(\\d+)(\\s)(\\w)\\:\\s(\\w+)").
% https://stackoverflow.com/questions/49249057/what-is-the-most-idiomatic-way-to-convert-a-string-to-characters-in-erlang
% -import(helpers, [readlines/1]).

% https://stackoverflow.com/questions/29615924/how-to-count-occurence-of-an-item-in-a-list

count_valid_passwords(L) -> count_valid_passwords(L, 0).
count_valid_passwords([{_, true}|T], TotalValids) -> 
  count_valid_passwords(T, TotalValids+1);
count_valid_passwords([_|T], TotalValids) -> 
  count_valid_passwords(T, TotalValids);
count_valid_passwords([], TotalValids) -> TotalValids.

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
			get_all_lines(Device, [N | Lines])
	end.


is_password_valid({Min, Max, Char, Password}) -> 
  Count = count(Char, Password),
  % io:format("Password: ~p // Min: ~p // Max: ~p // Count: ~p // Char: ~p~n", [Password, Min, Max, Count, Char]),
  V1 = Count >= Min,
  V2 = Count =< Max,%Max >= Count,
  V1 and V2.
  


count(Char, String) -> 
  [UnicodeChar|_] = string:next_codepoint( Char ),
  count(UnicodeChar, String, 0).

count(Char, [Char|T], Count)  -> count(Char, T, Count+1);
count(Char, [_|T], Count) -> count(Char, T, Count);
count(_Char, [], Count) -> Count.

decompose(Rule) -> 
  {match, [[Min, Max, Letter, String]]} = re:run(Rule,?REGEX,[global,{capture,[1, 2, 4, 5],list}]),
  {
    list_to_integer(Min),
    list_to_integer(Max),
    Letter,
    String
  }.

run() -> 
  L = readlines("input.txt"),
  Result = [
    {decompose(Rule), is_password_valid( decompose(Rule) )}
    || Rule <- L],
    count_valid_passwords(Result).
  % {Result, count_valid_passwords(Result)}.

  % {Min, Max, Letter, String} = decompose(re:run("1-3 b: cdefg",?REGEX,[global,{capture,[1, 2, 4, 5],list}])),
  % is_password_valid(Min, Max, Letter, String).
  % {match, [[Min, Max, Letter, String]]} = re:run("5-10 a: aaa",?REGEX,[global,{capture,[1, 2, 4, 5],list}]),
  % {
  %   list_to_integer(Min),
  %   list_to_integer(Max),
  %   Letter,
  %   String
  % }.
  % re:run("5-10 a: aaa","(\\d+)-(\\d+)(\\s)(\\w)\\:\\s(\\w+)",[global,{capture,[1, 2, 4, 5],list}]).