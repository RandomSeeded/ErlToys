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

handle_info({start_worker_supervisor, Sup, MFA}, S = #state{}) ->
  {ok, Pid} = supervisor:start_child(Sup, ?SPEC(MFA)),
  link(Pid),
  {noreply, S#state{sup=Pid}};
handle_info(Msg, State) ->
  io:format("Unknown msg: ~p~n", [Msg]),
  {noreply, State}.

