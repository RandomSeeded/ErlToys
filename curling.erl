-module(curling).
-compile(export_all).

% FUNDAMENTALLY WHAT IS GEN_EVENT?
% It's a pub-sub model.

start_link(TeamA, TeamB) ->
  {ok, Pid} = gen_event:start_link(),
  %% The scoreboard will always be there
  gen_event:add_handler(Pid, curling_scoreboard, []),
  set_teams(Pid, TeamA, TeamB),
  {ok, Pid}.
 
set_teams(Pid, TeamA, TeamB) ->
  gen_event:notify(Pid, {set_teams, TeamA, TeamB}).
 
add_points(Pid, Team, N) ->
  gen_event:notify(Pid, {add_points, Team, N}).
 
next_round(Pid) ->
  gen_event:notify(Pid, next_round).

join_feed(Pid, ToPid) ->
  HandlerId = {curling_feed, make_ref()},
  gen_event:add_handler(Pid, HandlerId, [ToPid]),
  HandlerId.

leave_feed(Pid, HandlerId) ->
  gen_event:delete_handler(Pid, HandlerId, leave_feed).


