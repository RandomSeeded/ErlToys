-module(ppool_nagger).
-behavior(gen_server).
-compile(export_all).

start_link(Task, Delay, Max, SendTo) ->
  io:format("ppool_nagger start_link~n"),
  gen_server:start_link(?MODULE, {Task, Delay, Max, SendTo}, []).

stop(Pid) ->
  gen_server:call(Pid, stop).

init({Task, Delay, Max, SendTo}) ->
  io:format("ppol_nagger init~n"),
  {ok, {Task, Delay, Max, SendTo}, Delay}.

handle_call(stop, _From, State) ->
  {stop, normal, ok, State};
handle_call(_Msg, _From, State) ->
  {noreply, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(timeout, {Task, Delay, Max, SendTo}) ->
  SendTo ! {self(), Task},
  if Max =:= infinity ->
      {noreply, {Task, Delay, Max, SendTo}, Delay};
    Max =< 1 ->
      {stop, normal, {Task, Delay, 0, SendTo}};
    Max > 1 ->
      {noreply, {Task, Delay, Max-1, SendTo}, Delay}
  end.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

terminate(_Reason, _State) ->
  ok.

