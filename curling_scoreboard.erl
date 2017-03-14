-module(curling_scoreboard).
-behavior(gen_event).

-compile(export_all).

% Module responsible for forwarding events to the hardware module
% Events come from gen_event:notify/2
init([]) ->
  {ok, []}.

handle_event({set_teams, TeamA, TeamB}, State) ->
  curling_scoreboard_hw:set_teams(TeamA, TeamB),
  {ok, State};
handle_event({add_points, Team, N}, State) ->
  [curling_scoreboard_hw:add_point(Team) || _ <- lists:seq(1, N)],
  {ok, State};
handle_event(next_round, State) ->
  curling_scoreboard_hw:next_round(),
  {ok, State}.

handle_call(_, State) ->
  {ok, ok, State}.

handle_info(_, State) ->
  {ok, State}.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

terminate(_Reason, _State) ->
  ok.

% OK so annoying thing is anybody using this is calling gen_event directly
% In order to make that less lame (they need to know our protocol), we can provide helper methods which issue the `gen_event:notify(X, Y)` calls for us.
% Nothing new there!



