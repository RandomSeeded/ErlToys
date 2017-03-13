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

trade(OwnPid, OtherPid) ->
  gen_fsm:sync_send_event(OwnPid, {negotiate, OtherPid}, 30000). % we use atoms for tuples! Basically the pattern here is we have abstracted to (To, Message). But sometimes we need more info in that message

accept_trade(OwnPid) ->
  gen_fsm:sync_send_event(OwnPid, accept_negotiate). % When we're communicating with our OWN pid we don't need to worry about race conditions. Client to process, no problem. Use sync.

% These on the other hand are exposed TO THE OTHER FSM. Given that we don't know what our own process is doing ATM< we communicate asynchronously.
ask_negotiate(OtherPid, OwnPid) ->
  io:format("Ask for negotiate"),
  gen_fsm:send_event(OtherPid, {ask_negotiate, OwnPid}).

accept_negotiate(OtherPid, OwnPid) ->
  gen_fsm:send_event(OtherPid, {accept_negotiate, OwnPid}).

% Now for callbacks from gen_fsm
init(Name) ->
  {ok, idle, #state{name=Name}}.

% Idle/2 takes in info from async. What about sync?
idle(_Event, Data) ->
  io:format("2 PARAM IDLE~n"),
  {next_state, idle, Data}.

% I would've THOUGHT we need idle3 for async because you need to know who to reply to if it's async
% However, maybe the paradigm here is that if's truly async we don't reply? That seems weird.
% OK SO: fundamentally the reason why you can / need to reply in the SYNCHRONOUS case is because what it means to be SYNCHRONOUS in erlang is that you have another process stuck in a loop waiting for you to respond.
% So we MUST respond to synchronous callers. If we don't use the {reply} tuple, the caller will break.
% The interesting thing is that StateName/3 actually CAN'T respond to its caller at all. It doesn't know who it was!
% But that seems to be the pattern. In the case of the kitty server, when we asynchronously returned a cat, we just told the running process it had a return and then we never waited for a response other than 'ok' (request sent) in any way.
% AHA! BUT! If we really needed to respond to this async event, we could pass in the _From Pid inside the tuple! Daaaang! We can always pass in as many 'arguments' as we want by modifying tuples hah.
idle(_Event, _From, Data) ->
  io:format("3 PARAM IDLE~n"),
  {reply, "Test", idle, Data}.







