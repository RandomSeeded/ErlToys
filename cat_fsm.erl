% This is useless but it's fun practice for standard setup
-module(cat_fsm).
-compile(export_all).

start() ->
  spawn(fun() -> dont_care() end).

event(Pid, Event) ->
  Ref = make_ref(),
  Pid ! {self(), Ref, Event},
  receive
    {Ref, Msg} ->
      {ok, Msg}
  after 5000 ->
      {error, timeout}
  end.

dont_care() ->
  receive
    {Pid, Ref, _Msg} ->
      Pid ! {Ref, meh};
    _ -> ok
  end,
  io:format("Switching to 'dont_care' state~n"),
  dont_care().
      



