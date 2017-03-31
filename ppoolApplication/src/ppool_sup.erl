-module(ppool_sup).
-behavior(supervisor).

-compile(export_all).

start_link(Name, Limit, MFA) ->
  io:format("ppool sup start link~n"),
  supervisor:start_link(?MODULE, {Name, Limit, MFA}).

% Starts the ppool server as a permanent worker
init({Name, Limit, MFA}) ->
  io:format("Init ~p~n", [Name]),
  MaxRestart = 1,
  MaxTime = 3600,
  {ok, {{one_for_all, MaxRestart, MaxTime},
        [{serv,
          {ppool_serv, start_link, [Name, Limit, self(), MFA]}, % name is passed onto the server, along with this supervisor's PID. This will allow the ppool_serv call for the spawning of the worker supervisor.
         permanent, 
         5000, %shutdown time
         worker,
         [ppool_serv]}]}}.

