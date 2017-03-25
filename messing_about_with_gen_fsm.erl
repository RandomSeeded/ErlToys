-module(trade_fsm).
-behavior(gen_fsm).

% This can be called from three places
% 1) Gen FSM (callbacks):
% export([init/1, handle_event/3,
%         handle_sync_event/4, handle_info/3,
%         terminate/3, code_change/4,
%         % Custon state names:
%         idle/2, idle/3, idle_wait/2, idle_wait/3,
%         negotiate/2, negotiate/3, wait/2, ready/2, ready/3]).

% 2) Public API (user)
% export([start/1, start_link/1, trade/2, 
%         accept_trade/1, make_offer/2, 
%         retract_offer/2, ready/1, cancel/1]).

% 3) Other FSM (person we are trading with)
-compile(export_all).

% -----------------
% PUBLIC API

% User invokes this
start(Name) ->
  gen_fsm:start(?MODULE, [Name], []).

% Gen FSM invokes this
init(_Name) ->
  {ok, idle, []}.

% Now Gen FSM will call this whenever gen FSM gets hit with an event.
idle(_Message, State) ->
  io:format("idle!~n"),
  % {reply, ok, idle, State}. % why doesn't this work?? very unclear. 
  {next_state, idle, State}.

% User-exposed method
do_a_thing(Pid) ->
  gen_fsm:send_event(Pid, test). % Does this invoke idle??

% This is for GLOBAL events (regardless of state)
handle_event(_Event, _StateName, _StateData) ->
  io:foramt("Global!~n"),
  ok.

