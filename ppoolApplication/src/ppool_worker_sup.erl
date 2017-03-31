-module(ppool_worker_sup).
-behavior(supervisor).
-compile(export_all).

start_link(MFA= {_,_,_}) ->
  supervisor:start_link(?MODULE, MFA).

init({M,F,A}) ->
  MaxRestart = 5,
  MaxTime = 3600,
  {ok, {{simple_one_for_one, MaxRestart, MaxTime}, % simple one for one because added potentially in high number, and we want to restrict their type (??? not sure what that means)
        [{ppool_worker, 
          {M,F,A}, % any OTP-compat will work
          temporary, 5000, worker, [M]}]}}. % all workers temporary; will be restarted on abnormal shutdown
% Temporary workers will actually allow us to offload the question fo whether or not they should be restarted onto the workers themselves. How would that work? Couldn't they crash? How would we then restart?

