-module(what_the_if).
-compile(export_all).

heh_fine() ->
  if 1 =:= 1 ->
      works
  end,
  if 1 =:= 2; 1 =:= 1 ->
    works
  end,
  if 1 =:= 2, 1 =:= 1 ->
      fails
  end.

oh_god(N) ->
  if N =:= 2 -> might_succeed;
    true -> always_does  %% this is Erlang's if's 'else!'
  end.
