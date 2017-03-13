-module(trade_fsm).
-behavior(gen_fsm).

-compile(export_all).

-record(state, {name="",
                other,
                ownitems=[],
                otheritems=[],
                monitor,
                from}).

start(Name) ->
  gen_fsm:start(?MODULE, [Name], []).

start_link(Name) ->
  gen_fsm:start_link(?MODULE, [Name], []).

% Synchronous call exposed to user (sync)
trade(OwnPid, OtherPid) ->
  gen_fsm:sync_send_event(OwnPid, {negotiate, OtherPid}, 30000).

accept_trade(OwnPid) ->
  gen_fsm:send_sync_event(OwnPid, accept_negotiate).

make_offer(OwnPid, Item) ->
  gen_fsm:send_event(OwnPid, {make_offer, Item}).

retract_offer(OwnPid, Item) ->
  gen_fsm:send_event(OwnPid, {retract_offer, Item}).

ready(OwnPid) ->
  gen_fsm:send_sync_event(OwnPid, ready, infinity).

cancel(OwnPid) ->
  gen_fsm:sync_send_all_state_event(OwnPid, cancel).

% FSM-to-FSM calls (async)
ask_negotiate(OtherPid, OwnPid) ->
  gen_fsm:send_event(OtherPid, {ask_negotiate, OwnPid}).

accept_negotiate(OtherPid, OwnPid) ->
  gen_fsm:send_event(OtherPid, {accept_negotiate, OwnPid}).

% Issue an offer to the other fsm
do_offer(OtherPid, Item) ->
  gen_fsm:send_event(OtherPid, {do_offer, Item}).

% Tell the other fsm the offer is retracted
undo_offer(OtherPid, Item) ->
  gen_fsm:send_event(OtherPid, {undo_offer, Item}).

% Query for ready to trade
are_you_ready(OtherPid) ->
  gen_fsm:send_event(OtherPid, are_you_ready).

not_yet(OtherPid) ->
  gen_fsm:send_event(OtherPid, not_yet).

am_ready(OtherPid) ->
  gen_fsm:send_event(OtherPid, 'ready!'). %kinda weird this is a string instead of an atom like the rest, but oh well

% Acknowledge that this FSM is in a ready state
ack_trans(OtherPid) ->
  gen_fsm:send_event(OtherPid, ack).

ask_commit(OtherPid) ->
  gen_fsm:sync_send_event(OtherPid, ask_commit).

do_commit(OtherPid) ->
  gen_fsm:sync_send_event(OtherPid, do_commit).

notify_cancel(OtherPid) ->
  gen_fsm:send_all_state_event(OtherPid, cancel).

% gen_fsm callbacks (includes states)
% Fundamentally, how will states handle differentiating between events? Case statement?
% Probably not, probably pattern matching against the args
init(Name) ->
  {ok, idle, #state{name=Name}}.

% This seems useful...
notice(#state{name=N}, Str, Args) ->
  io:format("~s: "++Str++"~n", [N|Args]).

% As does this
unexpected(Msg, State) ->
  io:format("~p received unknown event ~p while in state ~p~n", [self(), Msg, State]).

idle({ask_negotiate, OtherPid}, S=#state{}) ->
  Ref = monitor(process, OtherPid), % done in case of the other pid dying. What will we do with that?
  notice(S, "~p asked for a trade negotiation", [OtherPid]),
  {next_state, idle_wait, S#state{other=OtherPid, monitor=Ref}};
% The only ASYNC CALL (from the other process) we should ever receive while in the idle state is ask_negotiate. Otherwise err out
idle(Event, Data) ->
  unexpected(Event, idle),
  {next_state, idle, Data}.

% We can also receive sync calls though, from the user
idle({negotiate, OtherPid}, From, S=#state{}) ->
  ask_negotiate(OtherPid, self()),
  notice(S, "asking user ~p for a trade", [OtherPid]),
  Ref = monitor(process, OtherPid),
  {next_state, idle_wait, S#state{other=OtherPid, monitor=Ref, from=From}};
idle(Event, _From, Data) ->
  unexpected(Event, idle),
  {next_state, idle, Data}.
% BOTH OF THE ABOVE: don't actually respond to the client, which will cause it to hang. This is intended here, apparently. 
% I would probably do things slightly differently, but oh well.

% This is a very specific race condition case of both Pids requesting negotiation simultaneously. This raises the possibility of receiving an 'accept negotiate' message while already in the negotiate state. Oh well.
% A dumb alternative would be to not expect that and to drop back to idle in the case of anything unexpected, and start from scratch
idle_wait({ask_negotiate, OtherPid}, S=#state{other=OtherPid}) ->
  gen_fsm:reply(S#state.from, ok),
  notice(S, "starting negotiation", []),
  {next_state, negotiate, S};
%The below is the more expected version
idle_wait({accept_negotiate, OtherPid}, S=#state{other=OtherPid}) ->
  gen_fsm:reply(S#state.from, ok),
  notice(S, "starting negotiation", []),
  {next_state, negotiate, S};
idle_wait(Event, Data) ->
  unexpected(Event, idle_wait),
  {next_state, idle_wait, Data}.

% Isolate actions from implementations. If we later wanted to not use simple lists, we could simply sub the new implementation into these functions. THATS KINDA CONVENIENT
add(Item, Items) ->
  [Item | Items].

remove(Item, Items) ->
  Items -- [Item].

