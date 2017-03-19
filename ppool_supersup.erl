-module(ppool_supersup).
-behavior(supervisor).

-compile(export_all).

start_link() ->
  supervisor:start_link({local, ppool}, ?MODULE, []).

% Have to brutally kill supervisors. There is a better way apparently.
stop() ->
  case whereis(ppool) of
    P when is_pid(P) ->
      exit(P, kill);
    _ -> ok
  end.

% This is a top level supervisor. Doesn't have any children (workers).
% Its task is to hold pools in memory and supervise them, where the pools will also be supervisors.
init([]) ->
  MaxRestart = 6,
  MaxTime = 3600,
  {ok, {{one_for_one, MaxRestart, MaxTime}, []}}.

start_pool(Name, Limit, MFA) ->
  ChildSpec = {Name,
               {ppool_sup, start_link, [Name, Limit, MFA]}, % Programmer-submitted data is static to the pool sup
               permanent, 10500, supervisor, [ppool_sup]}, % pool sup is permanent, will always be restarted.
  supervisor:start_child(ppool, ChildSpec).

stop_pool(Name) ->
  supervisor:terminate_child(ppool, Name),
  supervisor:delete_child(ppool, Name).


