-module(ppool_server).
-behavior(gen_server).
-compile(export_all).

% OPERATIONS TO SUPPORT:
% running a task in the pool (or being told can't start if pool is full)
% Running a task in the pool if there's room, otherwise hold the calling process
% Running a task in the pool as soon as possible, otherwise queue

% Why do we need BOTH of the first two options? I think just generally useful to support both.
% I think this is the difference between 'run' and 'sync_queue'

start(Name, Limit, Sup, MFA) when is_atom(Name), is_integer(Limit) ->
  gen_server:start({local, Name}, ?MODULE, {Limit, MFA, Sup}, []).

start_link(Name, Limit, Sup, MFA) when is_atom(Name), is_integer(Limit) ->
  gen_server:start_link({local, Name}, ?MODULE, {Limit, MFA, Sup}, []).

run(Name, Args) ->
  gen_server:call(Name, {run, Args}).

sync_queue(Name, Args) ->
  gen_server:call(Name, {sync, Args}, infinity).

async_queue(Name, Args) ->
  gen_server:cast(Name, {async, Args}).

stop(Name) ->
  gen_server:call(Name, stop).

% A macro to define the child specification of the worker supervisor. 
-define(SPEC(MFA),
  {worker_sup,
    {ppool_worker_sup, start_link, [MFA]},
    temporary,
    10000,
    supervisor,
    [ppool_worker_sup]}).

% State: tracks the pid of the supervisor, number of processes that can run, a queue of jobs to run, and refs to the running processes
-record(state, {limit=0,
    sup,
    refs,
    queue=queue:new()}).

% WRONG! This init is basically going to be called by the sup, so we can't be calling it in term
% init({Limit, MFA, Sup}) ->
%   {ok, Pid} = supervisor:start_cdhild(Sup, ?SPEC(MFA)),
%   link(Pid),
%   {ok, #state{limit=Limit, refs=gb_sets:empty()}}.

init({Limit, MFA, Sup}) ->
  self() ! {start_worker_supervisor, Sup, MFA},
  {ok, #state{limit=Limit, refs=gb_sets:empty()}}. % Now the caller (ppool_sup) can continue.

handle_call({run, Args}, _From, S = #state{limit=N, sup=Sup, refs=R}) when N > 0 -> % limit represents the amount of remaining processes that can be added
  {ok, Pid} = supervisor:start_child(Sup, Args), % this doesn't mean start a supervisor. It means tell the supervisor (Sup) to start a child
  Ref = erlang:monitor(process, Pid),
  {reply, {ok, Pid}, S#state{limit=N-1, refs=gb_sets:add(Ref, R)}};
handle_call({run, _Args}, _From, S=#state{limit=N}) when N =< 0 ->
  {reply, noalloc, S}; % if we are using run, we don't do anything if we have no workers left in the pool
handle_call({sync, Args}, _From, S = #state{limit=N, sup=Sup, refs=R}) when N > 0 ->
  {ok, Pid} = supervisor:start_child(Sup, Args),
  Ref = erlang:monitor(process, Pid),
  {reply, {ok, Pid}, S#state{limit=N-1, refs=gb_sets:add(Ref, R)}}; % for sync run, same thing, we run if we can
handle_call({sync, Args}, From, S = #state{queue=Q}) ->
  {noreply, S#state{queue=queue:in({From, Args}, Q)}}; % but if we can't, we add it to the queue
handle_call(stop, _From, State) ->
  {stop, normal, ok, State};
handle_call(_Msg, _From, State) ->
  {noreply, State}.

handle_cast({async, Args}, S=#state{limit=N, sup=Sup, refs=R}) when N > 0 ->
  {ok, Pid} = supervisor:start_child(Sup, Args),
  Ref = erlang:monitor(process, Pid),
  {noreply, S#state{limit=N-1, refs=gb_sets:add(Ref, R)}};
handle_cast({async, Args}, S=#state{limit=N, queue=Q}) when N =< 0 -> % async if we can't add to the queue as well
  {noreply, S#state{queue=queue:in(Args,Q)}};
handle_cast(_Msg, State) ->
  {noreply, State}.

% When do we REMOVE from the queue? When it goes down!
handle_info({'DOWN', Ref, process, _Pid, _}, S = #state{refs=Refs}) ->
  io:format("received down msg~n"),
  case gb_sets:is_element(Refs, Refs) of
    true ->
      handle_down_worker(Ref, S);
    false -> % why would we receive a message from something that's not our worker?? Unclear. I think this is just extra safety.
      {noreply, S}
  end;
handle_info({start_worker_supervisor, Sup, MFA}, S = #state{}) ->
  {ok, Pid} = supervisor:start_child(Sup, ?SPEC(MFA)),
  link(Pid),
  {noreply, S#state{sup=Pid}};
handle_info(Msg, State) ->
  io:format("Unknown msg: ~p~n", [Msg]),
  {noreply, State}.


handle_down_worker(Ref, S = #state{limit=L, sup=Sup, refs=Refs}) ->
  case queue:out(S#state.queue) of
    {{value, {From, Args}}, Q} -> % sync! Now we make sure to reply to unblock that thread.
      {ok, Pid} = supervisor:start_child(Sup, Args),
      NewRef = erlang:monitor(process, Pid),
      NewRefs = gb_sets:insert(NewRef, gb_sets:delete(Ref, Refs)),
      gen_server:reply(From, {ok, Pid}),
      {noreply, S#state{refs=NewRefs, queue=Q}};
    {{value, Args}, Q} -> % async, we just start and don't care.
      {ok, Pid} = supervisor:start_child(Sup, Args),
      NewRef = erlang:monitor(process, Pid),
      NewRefs = gb_sets:insert(NewRef, gb_sets:delete(Ref, Refs)),
      {noreply, S#state{refs=NewRefs, queue=Q}};
    {empty, _} ->
      {noreply, S#state{limit=L+1, refs=gb_sets:delete(Ref,Refs)}}
  end.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

terminate(_Reason, _State) ->
  ok.



