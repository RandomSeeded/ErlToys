-module(musicians).
-compile(export_all).
-behavior(gen_server).

-record(state, {name="", role, skill=good}).
-define(DELAY, 750).

start_link(Role, Skill) ->
  gen_server:start_link({local, Role}, ?MODULE, [Role, Skill], []).

stop(Role) ->
  gen_server:call(Role, stop).

init([Role, Skill]) ->
  process_flag(trap_exit, true),
  random:seed(now()),
  TimeToPlay = random:uniform(3000),
  Name = pick_name(),
  StrRole = atom_to_list(Role),
  io:format("Musician ~s, playing the ~s, entered the room~n", [Name, StrRole]),
  {ok, #state{name=Name, role=StrRole, skill=Skill}, TimeToPlay}. % this is a kind of hacky way to produce a loop, IMO. What's going on is we are repeatedly triggering timeouts. The repeatedly is caused by handle_info also noreplying with a timeout. From the docs, "if {noreply, NewState} is returned, the gen_server process continues executing with NewState"

pick_name() ->
  %% the seed must be set for the random functions. Use within the
  %% process that started with init/1
  lists:nth(random:uniform(10), firstnames()) 
  ++ " " ++
  lists:nth(random:uniform(10), lastnames()).
 
firstnames() ->
  ["Valerie", "Arnold", "Carlos", "Dorothy", "Keesha",
  "Phoebe", "Ralphie", "Tim", "Wanda", "Janet"].
 
lastnames() ->
  ["Frizzle", "Perlstein", "Ramon", "Ann", "Franklin",
  "Terese", "Tennelli", "Jamal", "Li", "Perlstein"].

handle_call(stop, _From, S=#state{}) -> % If we're told to stop, we do it
  {stop, normal, ok, S};
handle_call(_Message, _From, S) -> % If we receive unexpected message, we don't respond, and our caller crashes.
  {noreply, S, ?DELAY}.

handle_cast(_Message, S) ->
  {noreply, S, ?DELAY}.

handle_info(timeout, S=#state{name=N, skill=good}) ->
  io:format("~s produced sound! ~n", [N]),
  {noreply, S, ?DELAY};
handle_info(timeout, S=#state{name=N, skill=bad}) ->
  case random:uniform(5) of
    1 ->
      io:format("~s played a bad note, Uh oh~n", [N]),
      {stop, bad_note, S};
    _ ->
      io:format("~s produced sound! ~n", [N]),
      {noreply, S, ?DELAY}
  end;
handle_info(_Message, S) ->
  {noreply, S, ?DELAY}.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

terminate(normal, S) -> % triggered by calling stop() -> gen_server:call(stop) -> {stop, ... } -> here
  io:format("~s left the room (~s)~n", [S#state.name, S#state.role]);
terminate(bad_note, S) -> % corresponds to crash. Our caller received a timeout, so we killed this process from the handle_info
  io:format("~s sucks! kicked that member out of the band! (~s)~n", [S#state.name, S#state.role]);
terminate(shutdown, S) -> % corresponds to the supervisor telling all processes to close
  io:format("Whole band shutdown, including ~s~n", [S#state.name]);
terminate(_Reason, S) -> % generic unexpected
  io:format("Unexpected shutdown catchall for ~s~n", [S#state.name]).





