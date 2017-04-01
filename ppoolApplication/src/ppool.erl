% Friendly API
-module(ppool).
-behavior(application).
-compile(export_all).

% These are from the original hand-started version
% start_link() ->
%   ppool_supersup:start_link().
% 
% stop() ->
%   ppool_supersup:stop().

% Now we can have OTP-app-compatible versions
start(normal, _Args) ->
  ppool_supersup:start_link(). % Needs to return {ok, Pid}...but that's the same as what supersup:s_l will do

stop(_State) -> % only does cleanup
  ok.

start_pool(Name, Limit, {M,F,A}) ->
  ppool_supersup:start_pool(Name, Limit, {M,F,A}).

stop_pool(Name) ->
  ppool_supersup:stop_pool(Name).

run(Name, Args) ->
  ppool_serv:run(Name, Args).

async_queue(Name, Args) ->
  ppool_serv:async_queue(Name, Args).

sync_queue(Name, Args) ->
  ppool_serv:sync_queue(Name, Args).



