-module(event_server).
-compile(export_all).
-record(state, {events,
    clients}).
-record(event, {name="",
    description="",
    pid,
    timeout={{1970,1,1},{0,0,0}}}).

loop(S = #state{}) ->
  receive
    {Pid, MsgRef, {subscribe, Client}} ->
      ok;
    {Pid, MsgRef, {add, Name, Description, Timeout}} ->
      ok;
    {Pid, MsgRef, {cancel, Name}} ->
      ok;
    {done, Name} ->
      ok;
    {shutdown} ->
      ok;
    {'DOWN', Ref, process, _Pid, _Reason} ->
      ok;
    code_change ->
      ok;
    Unknown ->
      io:format("Unknown message: ~p~n", [Unknown]),
      loop(S)
  end.

start() ->
  spawn(?MODULE, init, []).

start_link() ->
  spawn_link(?MODULE, init, []).

%% server keeps two things in state: list of subscribing clients, and list of all event processes it spawned
% description is stored in the event server; the events themselves don't need to care
init() ->
  loop(#state{events=orddict:new(),
      clients=orddict:new()}).



