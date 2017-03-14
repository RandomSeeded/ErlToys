-module(curling_scoreboard_hw).
-compile(export_all).

% Fake hardware module
set_teams(TeamA, TeamB) ->
  io:format("Scoreboard: Team ~s vs. Team ~s~n", [TeamA, TeamB]).

next_round() ->
  io:format("Scoreboard: round over~n").

add_point(Team) ->
  io:format("Scoreboard: increased score of team ~s by 1 ~n", [Team]).

reset_board() ->
  io:format("Scoreboard: All teams are undefined and all scores are 0~n").

