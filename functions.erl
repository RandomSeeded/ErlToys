-module(functions).
-compile(export_all).

% head(List) ->
%   [Head | _] = List.

head([Head|_]) -> Head.

% second([_|[S|[]]]) -> S.

% second([_|[S|_]]) -> S.

second([_, S|_]) -> S.

% JS WAY
% same(One, Two) ->
%   One =:= Two.

% ERL WAY
same(X, X) -> true;
same(_, _) -> false.
