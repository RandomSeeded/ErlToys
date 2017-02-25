-module(my_event).
% TODO (nw): fix the export all
-compile(export_all).

create_event(Pid, Identifier, Delay) ->
  spawn(?MODULE, event, [Pid, Identifier, Delay]).

% how are we planning to create these?
% Presumably with a name and a delay?
event(Pid, Identifier, Delay) ->
  receive
    _ -> ok
  after Delay ->
      Pid ! {ok, Identifier}
  end.
