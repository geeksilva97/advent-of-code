-module(advent2).
-compile(export_all).

-define(REGEX, "(\\d+)-(\\d+)(\\s)(\\w)\\:\\s(\\w+)").

% https://stackoverflow.com/questions/29615924/how-to-count-occurence-of-an-item-in-a-list

is_password_valid(Min, Max, Password) -> false.


% process_list(List) ->


run() -> 
  L = [
    "1-3 a: abcde",
    "1-3 b: cdefg",
    "2-9 c: ccccccccc"
  ],
  re:run("5-10 a: aaa",?REGEX,[global,{capture,[1, 2, 4, 5],list}]).
  % re:run("5-10 a: aaa","(\\d+)-(\\d+)(\\s)(\\w)\\:\\s(\\w+)",[global,{capture,[1, 2, 4, 5],list}]).