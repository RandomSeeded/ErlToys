-module(event).
-compile(export_all).
-record(state, {server,
    name="",
    to_go=[0|[]]}).

% So our full init stuff is pretty complicated. We call event:start which spawns a process
% This calls init, which is just a mildly helpful wrapper around loop (normalize delay)
% loop waits to be cancelled
% Loop only needs to be a loop because of giant timeouts
% When time is elapsed, it pipes an event to the server (which is the invoking process, from self())

loop(S = #state{server=Server, to_go=[T|Next]}) ->
  receive
    {Server, Ref, cancel} ->
      Server ! {Ref, ok}
  after T*1000 ->
      if Next =:= [] ->
          Server ! {done, S#state.name};
        Next =/= [] ->
          loop(#state{server=Server, to_go=Next})
      end
  end.

start(EventName, Delay) ->
  spawn(?MODULE, init, [self(), EventName, Delay]).

start_link(EventName, Delay) ->
  spawn_link(?MODULE, init, [self(), EventName, Delay]).

%% Turns N into a list of remainders on top of 49 days, because we can't timeout for longer than 50 days
normalize(N) -> 
  Limit = 49 * 24 * 60 * 60,
  % This will look something like [Remainder, Limit, Limit, Limit, Limit]
  [N rem Limit | lists:duplicate(N div Limit, Limit)].

time_to_go(TimeOut={{_,_,_}, {_,_,_}}) ->
    Now = calendar:local_time(),
    ToGo = calendar:datetime_to_gregorian_seconds(TimeOut) - calendar:datetime_to_gregorian_seconds(Now),
    Secs = if ToGo > 0 -> ToGo;
      ToGo =< 0 -> 0
    end,
    normalize(Secs);
time_to_go(Secs) ->
    normalize(Secs).

%% init: for innards?
init(Server, EventName, DateTime) ->
  loop(#state{server=Server,
      name=EventName,
      to_go=time_to_go(DateTime)}).

% so all these 'functions' really are are interfaces with the running process
% OK so the point of this is going to be so that we don't have to pipe messages into the thing directly. We don't need to know the format that this little process takes
% Instead we just invoke the method to deal with that complexity itself
cancel(Pid) ->
  % This checks to see if the Pid we are going to cancel is already dead
  Ref = erlang:monitor(process, Pid),
  % We then tell that process to cancel itself
  Pid ! {self(), Ref, cancel},
  receive
    {Ref, ok} ->
      % Removes the monitor on ref, so we won't be notified when it dies
      erlang:demonitor(Ref, [flush]);
    % In case we're notified of it dying before ^. Alternately if it died for some other reason (but not w/ EXIT I guess)
    {'DOWN', Ref, process, _Reason} ->
      ok
  end.


