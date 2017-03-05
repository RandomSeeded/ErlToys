-module(my_server).
-compile(export_all).

start(Module, InitialState) ->
  spawn(fun() -> init(Module, InitialState) end).
 
start_link(Module, InitialState) ->
  spawn_link(fun() -> init(Module, InitialState) end).

call(Pid, Msg) ->
  % Get a reference to the process to call
  % What's the point of doing this?
  % It's basically a message identifier. But how? Wouldn't it be repeated for multiple calls?
  % No it's recreated for each monitor. It's the monitor reference not the Pid
  % We could also just use make_ref BUT we'd need to set up the monitor anyway so we might as well use it.
  % What do we want the monitor for? In kitty server, error passthru
  Ref = erlang:monitor(process, Pid),
  % Pid ! {self(), Ref, Msg},

  % Now we distinguish between sync and async calls
  Pid ! {sync, self(), Ref, Msg},
  receive
    {Ref, Reply} ->
      erlang:demonitor(Ref, [flush]),
      Reply;
    {'DOWN', Reason} ->
      erlang:error(Reason)
  after 5000 ->
      erlang:error(timeout)
  end.

% This is for async
cast(Pid, Msg) ->
  Pid ! {async, Msg},
  ok.

init(Module, InitialState) ->
  loop(Module, Module:init(InitialState)).

loop(Module, State) ->
  receive
    {async, Msg} ->
      loop(Module, Module:handle_cast(Msg, State));
    {sync, Pid, Ref, Msg} ->
      loop(Module, Module:handle_call(Msg, Pid, Ref, State))
  end.


