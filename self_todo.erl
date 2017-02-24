-module(self_todo).
-compile(export_all).

start_server() ->
  spawn(?MODULE, restarter, []).

restarter() ->
  process_flag(trap_exit, true),
  Pid = spawn_link(?MODULE, event_server, []),
  register(event_server, Pid),
  receive
    {'EXIT', Pid, normal} ->
      ok;
    {'EXIT', Pid, shutdown} ->
      ok;
    {'EXIT', Pid, _} ->
      restarter()
  end.

event_server() ->
  ok.

