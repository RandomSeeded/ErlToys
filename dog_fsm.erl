-module(dog_fsm).
-export([start/0, squirrel/1, pet/1]).
 
% Client APIs
start() ->
  spawn(fun() -> bark() end).
 
squirrel(Pid) -> Pid ! squirrel.
 
pet(Pid) -> Pid ! pet.

% THOUGHTS
% So basically we have THREE loops for our different states. Huh.
% And then each loop handles the messages it receives differently
% If we wanted to, we could instead have a single state loop,

% Other thing to note: all functions here are async. Doesn't make any difference. We can do either.
% We could also have GLOBAL EVENTS: which happen in any state and have same action. How would we handle given that state is set up like this?
 
% Internal functions
bark() ->
  io:format("Dog says: BARK! BARK!~n"),
  receive
    pet ->
      wag_tail();
    _ ->
      io:format("Dog is confused~n"),
      bark()
  after 2000 ->
      bark()
  end.
 
wag_tail() ->
  io:format("Dog wags its tail~n"),
  receive
    pet ->
      sit();
    _ ->
      io:format("Dog is confused~n"),
      wag_tail()
  after 30000 ->
      bark()
  end.
 
sit() ->
  io:format("Dog is sitting. Gooooood boy!~n"),
  receive
    squirrel ->
      bark();
    _ ->
      io:format("Dog is confused~n"),
      sit()
  end.
