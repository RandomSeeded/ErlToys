-module(kitty_server).
-compile(export_all).

-record(cat, {name,
    color=green,
    description}).

%% Client API
start_link() ->
  spawn_link(fun init/0).

% Synchronous call (blocking)
% So this is actually pretty interesting in that the call is given a Pid to invoke with.
% This seems to follow the idea that generally state is maintained outside the module
% We don't have a singular process that we're going to use, instead we have functions which don't depend on state and passed all their dependencies
% So: why do we not do that in the todo app? Because in the todo app we are sure that we will only ever want a single event server. Therefore it makes sense to pipe to ?MODULE
% That pattern will also make sense for our surfAlert. However, is less extensible than this pattern
% UTILIZATION OF THIS PATTERN:
% Pid = kitty_server:start_link()
% kitty_server:order_cat(Pid, ...)
% UTILIZATION OF OTHER PATTERN:
% evserv:start()
% evserv:do_shit(...) [no Pid needed]

% PUBLIC METHODS
% Synchronous call
order_cat(Pid, Name, Color, Description) ->
  Ref = erlang:monitor(process, Pid),
  Pid ! {self(), Ref, {order, Name, Color, Description}},
  receive
    {Ref, Cat} ->
      erlang:demonitor(Ref, [flush]),
      Cat;
    {'DOWN', Ref, process, Pid, Reason} ->
      erlang:error(Reason)
  after 5000 ->
      erlang:error(timeout)
  end.

% Asynchronous call (nonblocking)
return_cat(Pid, Cat = #cat{}) ->
  Pid ! {return, Cat},
  ok.

% Async call
close_shop(Pid) ->
  Ref = erlang:monitor(process, Pid),
  Pid ! {self(), Ref, terminate},
  receive
    {Ref, ok} ->
      erlang:demonitor(Ref, [flush]),
      ok;
    {'DOWN', Ref, process, Pid, Reason} ->
      erlang:error(Reason)
  after 5000 ->
      erlang:error(timeout)
  end.

% Server internals
init() ->
  loop([]).

loop(Cats) ->
  receive
    {Pid, Ref, {order, Name, Color, Description}} ->
      if Cats =:= [] ->
          Pid ! {Ref, make_cat(Name, Color, Description)},
          loop(Cats);
        Cats =/= [] ->
          Pid ! {Ref, hd(Cats)},
          loop(tl(Cats))
      end;
    {return, Cat = #cat{}} ->
      loop([Cat|Cats]);
    {Pid, Ref, terminate} ->
      Pid ! {Ref, ok},
      terminate(Cats);
    Unknown ->
      io:format("Unknown message ~p~n", [Unknown]),
      loop(Cats)
  end.

% Private methods
make_cat(Name, Col, Desc) ->
  #cat{name=Name, color=Col, description=Desc}.

terminate(Cats) ->
  [io:format("~p was set free.~n",[C#cat.name]) || C <- Cats],
  ok.























