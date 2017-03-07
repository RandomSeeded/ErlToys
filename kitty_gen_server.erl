-module(kitty_gen_server).
-behavior(gen_server).
-compile(export_all).
-record(cat, {name, color=green, description}).  

% Client functions - For use by the outside world
start_link() ->
  gen_server:start_link(?MODULE, [], []).

order_cat(Pid, Name, Color, Description) ->
  io:format("Ordering cat~n"),
  gen_server:call(Pid, {order, Name, Color, Description}).

return_cat(Pid, Cat=#cat{}) ->
  gen_server:cast(Pid, {return, Cat}).

close_shop(Pid) ->
  gen_server:call(Pid, terminate).

% Server functions - (callbacks for OTP)
init([]) -> {ok, []}.

handle_call({order, Name, Color, Description}, _From, Cats) ->
  if Cats =:= [] ->
      {reply, make_cat(Name, Color, Description), Cats}; % The final parameter of these replies are interesting. We basically provide the new state for the next iteration of loop(), which now must live inside OTP
    Cats =/= [] ->
      {reply, hd(Cats), tl(Cats)}
  end;
handle_call(terminate, _From, Cats) ->
  {stop, normal, ok, Cats}.

handle_cast({return, Cat=#cat{}}, Cats) ->
  {noreply, [Cat|Cats]}.

handle_info(Msg, Cats) ->
  io:format("Unexpected message: ~p~n",[Msg]),
  {noreply, Cats}.

terminate(normal, Cats) ->
  [io:format("~p was set free.~n",[C#cat.name]) || C <- Cats],
  ok.

code_change(_OldVsn, State, _Extra) ->
  %% No change planned. The function is there for the behaviour,
  %% but will not be used. Only a version on the next
  {ok, State}.

% Private functions - for internal use
make_cat(Name, Col, Desc) ->
  #cat{name=Name, color=Col, description=Desc}.


