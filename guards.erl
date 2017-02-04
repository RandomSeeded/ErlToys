-module(guards).
-compile(export_all).

% JS WAY
% old_enough(X) ->
%   X >= 16.

% ERL WAY
old_enough(X) when X >= 16, X < 80 -> true;
old_enough(_) -> false.
