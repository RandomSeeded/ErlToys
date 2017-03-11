-module(trade_fsm).
-behavior(gen_fsm).

% This can be called from three places
% 1) Gen FSM
% 2) Public API (user)
% 3) Other FSM (person we are trading with)




