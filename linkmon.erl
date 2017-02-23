-module(linkmon).
-compile(export_all).

start_critic() ->
  spawn(?MODULE, critic, []).

start_critic2() ->
  spawn(?MODULE, restarter, []).

% So how does this all fit together?
% When we call start_critic2, it says spawn me a process that runs restarter w/ no args
% All that does is be a supervisor that does two things:
% 1) sets global process identifier 'critic' to point to our critic
% 2) listens for exits
% When the process exits unexpectedly, it restarts it, and redoes ^

% THEN, when we invoke judge3, it asks what process to send the info to. the globally registered critic, yo
% We then pipe a message to that critic telling it to do things, and we then get back a message when it's done
% For bonus points, we tell that critic to identify itself with a unique identifier instead of with its own process (basically a requestID), which handles some race conditions around us having an incorrect understanding of what its processId is.
% We COULD just listen on {_, Criticism}, which would fix that problem, but would be clearly problematic for larger systems because we would have no idea what request we just got a response to!
restarter() ->
  process_flag(trap_exit, true),
  Pid = spawn_link(?MODULE, critic2, []),
  register(critic, Pid),
  receive
    {'EXIT', Pid, normal} ->
      ok;
    {'EXIT', Pid, shutdown} ->
      ok;
    {'EXIT', Pid, _} -> 
      restarter()
  end.

judge3(Band, Album) ->
  % So what's the change we made here?
  % We passed a reference into our judge function. What is this ref for?
  % the reference is basically a UUID
  % so we told the critic 'do some shit and then send me a message, along with the UUID
  % why do we need this UUID? We don't even use it!
  % so the issue we're trying to avoid is that we can't match against {Pid, Criticism}
  % And the reason for that is because Pid = critic, where critic is a global variable which doesnt get updated when critic gets refreshed, only when its initially created
  % Now, we instead match against {Ref, Criticism}, where ref is created every time we pipe a message to our critic
  Ref = make_ref(),
  critic ! {self(), Ref, {Band, Album}},
  receive
    {Ref, Criticism} -> Criticism
  after 2000 ->
      timeout
  end.

% Has race conditions
judge2(Band, Album) ->
  critic ! {self(), {Band, Album}},
  Pid = whereis(critic),
  receive
    {Pid, Criticism} -> Criticism
  after 2000 ->
      timeout
  end.

judge(Pid, Band, Album) ->
  Pid ! {self(), {Band, Album}},
  receive 
    {Pid, Criticism} -> Criticism
  after 2000 ->
      timeout
  end.

% now with UUID support :shrug:
% so this is a replacement for self(). What's the point?
critic2() ->
  receive
    {From, Ref, {"Rage Against the Machine", "Unit Testify"}} -> 
      From ! {Ref, "!!!"};
    {From, Ref, {"Band2", "Album2"}} -> 
      From ! {Ref, "222"};
    {From, Ref, {_, _}} -> 
      From ! {Ref, "sadness"}
  end,
  critic2().


critic() ->
  receive
    {From, {"Rage Against the Machine", "Unit Testify"}} -> 
      From ! {self(), "!!!"};
    {From, {"Band2", "Album2"}} -> 
      From ! {self(), "222"};
    {From, {_, _}} -> 
      From ! {self(), "sadness"}
  end,
  critic().

