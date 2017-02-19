-module(dolphins).
-compile(export_all).

dolphin1() ->
  receive
    do_a_flip ->
      io:format("No~n");
    fish ->
      io:format("asdfa~n");
    _ ->
      io:format("other~n")
  end.

dolphin2() ->
  receive
    {From, do_a_flip} ->
      From ! "Flip";
    {From, _} ->
      From ! "asdfasdf"
  end.

dolphin3() ->
  receive
    {From, do_a_flip} ->
      From ! "Flip",
      dolphin3();
    {From, _} ->
      From ! "asdfasdf",
      dolphin3()
  end.
