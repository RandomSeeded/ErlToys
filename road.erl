-module(road).
-compile(export_all).

main() ->
  File = "road.txt",
  {ok, Bin} = file:read_file(File),
  parse_map(Bin).

parse_map(Bin) when is_binary(Bin) ->
  parse_map(binary_to_list(Bin));
parse_map(Str) when is_list(Str) ->
  Values = [list_to_integer(X) || X <- string:tokens(Str, "\r\n\t")],
  Map = group(Values, []),
  optimal_path(Map).

% The next step in parsing the map would be to regroup the data into the {A,B,X} form described earlier. Sadly, there's no simple generic way to pull elements from a list 3 at a time, so we'll have to pattern match our way in a recursive function in order to do it:
% Version 1
group([], Acc) -> lists:reverse(Acc);
group([A,B,X|Rest], Acc) -> group(Rest, [{A,B,X}|Acc]).

% Version 0 working (should be reversed)
% group(Accumulator, []) -> Accumulator;
% group(Accumulator, [A|[B|[X|Remainder]]]) ->
%   group([{group,{A,B,X}}|Accumulator], Remainder).
% group(Accumulator, [A|[B|[X|[Remainder]]]]) ->
%   group([{group,{A,B,X}}|Accumulator], Remainder).

% This gives us all the parameters our function will need: the {A,B,X} triples and an accumulator of the form {{DistanceA,PathA}, {DistanceB,PathB}}.
shortest_step({A,B,X}, {{DistA,PathA}, {DistB,PathB}}) ->
  OptA1 = {DistA + A, [{a, A}|PathA]},
  OptA2 = {DistB + B + X, [{x, X}, {b, B}|PathB]},
  OptB1 = {DistB + B, [{b, B}|PathB]},
  OptB2 = {DistA + A + X, [{x, X}, {a, A}|PathA]},
  {erlang:min(OptA1, OptA2), erlang:min(OptB1, OptB2)}.

optimal_path(Map) ->
  {A,B} = lists:foldl(fun shortest_step/2, {{0, []}, {0, []}}, Map),
  {_Dist, Path} = if hd(element(2,A)) =/= {x,0} -> A;
                     hd(element(2,B)) =/= {x,0} -> B
                  end,
  lists:reverse(Path).

