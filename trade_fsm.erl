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
  % We use gen_fsm:reply when we are replying to an async call.
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

% From our client: tell the other FSM about what our client's action
negotiate({make_offer, Item}, S=#state{ownitems=OwnItems}) ->
  do_offer(S#state.other, Item),
  notice(S, "offering ~p", [Item]),
  {next_state, negotiate, S#state{ownitems=add(Item, OwnItems)}};
negotiate({retract_offer, Item}, S=#state{ownitems=OwnItems}) ->
  undo_offer(S#state.other, Item),
  notice(S, "cancelling offer on ~p", [Item]),
  {next_state, negotiate, S#state{ownitems=remove(Item, OwnItems)}};
% % From the other FSM: notify our user, update our state
negotiate({do_offer, Item}, S=#state{otheritems=OtherItems}) ->
  notice(S, "other player offering ~p", [Item]),
  {next_state, negotiate, S#state{otheritems=add(Item, OtherItems)}};
negotiate({undo_offer, Item}, S=#state{otheritems=OtherItems}) ->
  notice(S, "Other player cancelling offer on ~p", [Item]),
  {next_state, negotiate, S#state{otheritems=remove(Item, OtherItems)}};
negotiate(are_you_ready, S=#state{other=OtherPid}) ->
  io:format("Other user ready to trade.~n"),
  notice(S, "Other user ready to trade goods:~n" 
         "You get ~p, The other side gets ~p", [S#state.otheritems, S#state.ownitems]),
  not_yet(OtherPid), % we're not yet ready if we're still in negotiate state.
  {next_state, negotiate, S}; %This basically separates the reply (not yet ready) from any state transition
negotiate(Event, Data) ->
  unexpected(Event, negotiate),
  {next_state, negotiate, Data}.

% From our client: synchronous! This is because we dont want user to modify offer after claiming readiness
% This seems not-ideal...now our client is hanging and unable to do anything after he's declared readiness. =/
negotiate(ready, From, S=#state{other=OtherPid}) ->
  are_you_ready(OtherPid),
  notice(S, "asking if ready, waiting", []),
  {next_state, wait, S#state{from=From}};
negotiate(Event, _From, S) ->
  unexpected(Event, negotiate),
  {next_state, negotiate, S}.

% If the other user changes the deal, we roll back from the wait state back into the negotiate state.
wait({do_offer, Item}, S=#state{otheritems=OtherItems}) ->
  gen_fsm:reply(S#state.from, offer_changed),
  notice(S, "other side offering ~p", [Item]),
  {next_state, negotiate, S#state{otheritems=add(Item, OtherItems)}};
wait({undo_offer, Item}, S=#state{otheritems=OtherItems}) ->
  gen_fsm:reply(S#state.from, offer_changed), % What's the difference between state.from and state.monitor?
  notice(S, "Other side cancelling offer of ~p", [Item]),
  {next_state, negotiate, S#state{otheritems=remove(Item, OtherItems)}};
wait(are_you_ready, S=#state{}) ->
  am_ready(S#state.other),
  notice(S, "asked if ready, and I am. Waiting for same reply", []),
  {next_state, wait, S};
wait(not_yet, S=#state{}) ->
  notice(S, "Other not ready yet", []),
  {next_state, wait, S};
wait('ready!', S=#state{}) ->
  am_ready(S#state.other),
  ack_trans(S#state.other),
  gen_fsm:reply(S#state.from, ok),
  notice(S, "other side is ready. Moving to ready state", []),
  {next_state, ready, S};
wait(Event, Data) ->
  unexpected(Event, wait),
  {next_state, wait, Data}.

% Async listener for initial ready state. Will issue sync calls to other FSM.
ready(ack, S=#state{}) ->
  case priority(self(), S#state.other) of
    true ->
      try
        notice(S, "asking for commit", []),
        ready_commit = ask_commit(S#state.other),
        notice(S, "ordering commit", []),
        ok = do_commit(S#state.other),
        notice(S, "committing...", []),
        commit(S),
        {stop, normal, S}
      catch Class:Reason ->
              notice(S, "commit failed", []),
              {stop, {Class, Reason}, S}
      end;
    false ->
      {next_state, ready, S} % do nothing, stay in ready state
  end;
ready(Event, Data) ->
  unexpected(Event, ready),
  {next_state, ready, Data}.

% Sync listeners (from FSMs).
ready(ask_commit, _From, S) ->
  notice(S, "replying to ask_commit", []),
  {reply, ready_commit, ready, S};
ready(do_commit, _From, S) ->
  notice(S, "committing...", []),
  commit(S),
  {stop, normal, ok, S};
ready(Event, _From, Data) ->
  unexpected(Event, ready),
  {next_state, ready, Data}.

commit(S = #state{}) ->
  io:format("Transaction completed for ~s. "
            "Items sent are:~n~p,~n received are:~n~p.~n"
            "This operation should have some atomic save "
            "in a database.~n",
            [S#state.name, S#state.ownitems, S#state.otheritems]).

% Other player cancelled trade, stop the trade.
handle_event(cancel, _StateName, S=#state{}) ->
  notice(S, "received cancel event", []),
  {stop, other_cancelled, S};
handle_event(Event, StateName, Data) -> % How does this work? Must basically be a try-catch against actual states, and this is only used if no states matched?
  % No. Handle event is only ever triggered when there was a send_all / sync_send_all. This will not trigger if you do a send_event against a state that does not exist.
  unexpected(Event, StateName),
  {next_state, StateName, Data}.

handle_sync_event(cancel, _From, _StateName, S=#state{}) ->
  notify_cancel(S#state.other),
  notice(S, "cancelling trade, sending cancel event", []),
  {stop, cancelled, ok, S};
handle_sync_event(Event, _From, StateName, Data) ->
  % This is "let it crash" in action. If we get something unexpected, we won't respond at all. Given that this is a synchronous call, this means our caller will probably crash, eventually, from a timeout.
  unexpected(Event, StateName),
  {next_state, StateName, Data}.

handle_info({'DOWN', Ref, process, Pid, Reason}, _, S=#state{other=Pid, monitor=Ref}) ->
  notice(S, "other side dead", []),
  {stop, {other_down, Reason}, S};
handle_info(Info, StateName, Data) ->
  unexpected(Info, StateName),
  {next_state, StateName, Data}.

code_change(_OldVsn, StateName, Data, _Extra) ->
  % Migrations would go here.
  {ok, StateName, Data}.

terminate(normal, ready, S=#state{}) ->
  notice(S, "FSM leaving.", []);
terminate(_Reason, _StateName, _StateData) ->
  ok.

% Isolate actions from implementations. If we later wanted to not use simple lists, we could simply sub the new implementation into these functions. THATS KINDA CONVENIENT
% In web dev land, this is where our models come into play
add(Item, Items) ->
  [Item | Items].

remove(Item, Items) ->
  Items -- [Item].

% Sorting allows us to avoid deadlock; only one pid will have priority and start the two-phase commit
priority(OwnPid, OtherPid) when OwnPid > OtherPid ->
  true;
priority(_OwnPid, _OtherPid) ->
  false.

% This seems useful...
notice(#state{name=N}, Str, Args) ->
  io:format("~s: "++Str++"~n", [N|Args]).

% As does this
unexpected(Msg, State) ->
  io:format("~p received unknown event ~p while in state ~p~n", [self(), Msg, State]).

