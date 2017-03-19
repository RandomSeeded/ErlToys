-module(curling_feed).
-behavior(gen_event).

-compile(export_all).

init([Pid]) ->
  {ok, Pid}.

% OK so order of operations is
% join_feed added a listener to the Pid. So anything sent to that Pid with gen_event:notify will get forwarded to the Pid that joined
% When we do curling:add_points, what happens?
% It calls gen_event:notify
% gen_event:notify calls ALL THE HANDLERS (is that right?)
% including this handler down below
% And all that handler does is forward that message to everybody in the feed. 
% How does it forward to all of them? Doesn't it send to only one?
% Because join_feed actually adds a new handler for every single subscribing Pid
% and the below handle_event is actually invoked with a Pid to send shit to.
handle_event(Event, Pid) ->
  Pid ! {curling_feed, Event},
  {ok, Pid}.

handle_call(_, State) ->
  {ok, ok, State}.
 
handle_info(_, State) ->
  {ok, State}.
 
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
 
terminate(_Reason, _State) ->
  ok.
