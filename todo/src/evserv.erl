-module(evserv).
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
      Ref = erlang:monitor(process, Client),
      NewClients = orddict:store(Ref, Client, S#state.clients),
      Pid ! {MsgRef, ok},
      loop(S#state{clients=NewClients});
    {Pid, MsgRef, {add, Name, Description, Timeout}} ->
      EventPid = events:start_link(Name, Timeout),
      NewEvents = orddict:store({name=Name,
                                 description=Description,
                                 pid=EventPid,
                                 timeout=Timeout}, MsgRef, S#state.events),
      Pid ! {MsgRef, ok},
      loop(S#state{events=NewEvents});
    {Pid, MsgRef, {cancel, Name}} ->
      Events = case orddict:find(Name, S#state.events) of
                 {ok, E} ->
                   event:cancel(E#event.pid),
                   orddict:erase(Name, S#state.events);
                 error ->
                   S#state.events
               end,
      Pid ! {MsgRef, ok},
      loop(S#state{events=Events});
    {done, Name} ->
      %% This comes from the events to tell us it's done
      case orddict:find(Name, S#state.events) of
        {ok, E} ->
          send_to_clients({done, E#event.name, E#event.description}, S#state.clients),
          NewEvents = orddist:erase(Name, S#state.events),
          loop(S#state{events=NewEvents});
        error ->
          loop(S)
      end;
    {shutdown} ->
      exit(shutdown);
    {'DOWN', Ref, process, _Pid, _Reason} ->
      % Received from the clients when they go down (unsubscribe)
      loop(S#state{clients=orddicts:erase(Ref,S#state.clients)});
    code_change ->
      ?MODULE:loop(S);
    Unknown ->
      io:format("Unknown message: ~p~n", [Unknown]),
      loop(S)
  end.

send_to_clients(Msg, ClientDict) ->
  orddict:map(fun(_Ref, Pid) -> Pid ! Msg end, ClientDict).

valid_datetime({Date,Time}) ->
  try
    calendar:valid_date(Date) andalso valid_time(Time)
  catch
    error:function_clause ->
      false
  end;
valid_datetime(_) ->
  false.

valid_time({H,M,S}) ->
  valid_time(H,M,S).
valid_time(H,M,S) when H >= 0, H < 24, 
                          M >=0, M < 60,
                          S >= 0, S < 60 ->
  true;
valid_time(_,_,_) ->
  false.

start() ->
  % spawn(?MODULE, init, []).
  % Why register? Because we should only have one event server running
  register(?MODULE, Pid=spawn(?MODULE, init, [])),
  Pid.

start_link() ->
  % spawn_link(?MODULE, init, []).
  register(?MODULE, Pid=spawn_link(?MODULE, init, [])),
  Pid.

subscribe(Pid) ->
  % How do you reference the running process? Don't we need a named process for that? A: we've got one
  Ref = erlang:monitor(process, whereis(?MODULE)),
  % That process is expecting:
  % {Pid, MsgRef, {subscribe, Client}} ->
  % What are each of these?
  % Pid = calling Pid
  % MsgRef = a UUID? Verifies that we're talking about the same message?
  % Client = the PID to send the messages to in the end
  % How does this work? What's the difference between ?MODULE and Ref?
  % Ref is a monitor, ?MODULE is raw?
  ?MODULE ! {self(), Ref, {subscribe, Pid}},
  receive
    {Ref, ok} ->
      {ok, Ref};
    {'DOWN', Ref, process, _Pid, Reason} ->
      {error, Reason}
  after 5000 ->
          {error, timeout}
  end.

add_event(Name, Description, Timeout) ->
  Ref = make_ref(),
  % {Pid, MsgRef, {add, Name, Description, Timeout}} ->
  ?MODULE ! {self(), Ref, {add, Name, Description, Timeout}},
  receive
    {Ref, Msg} -> Msg
  after 5000 ->
          {error, timeout}
  end.

cancel(Name) ->
  Ref = make_ref(),
  ?MODULE ! {self(), Ref, {cancel, Name}},
  receive
    {Ref, ok} -> ok
  after 5000 ->
          {error, timeout}
  end.

listen(Delay) ->
  receive
    M = {done, _Name, _Description} ->
      [M | listen(0)]
  after Delay*1000 ->
          []
  end.
 

%% server keeps two things in state: list of subscribing clients, and list of all event processes it spawned
% description is stored in the event server; the events themselves don't need to care
init() ->
  loop(#state{events=orddict:new(),
      clients=orddict:new()}).

