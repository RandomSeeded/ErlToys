-module(kitchen).
-compile(export_all).

% fridge1() ->
%   receive
%     {From, {store, Food}} ->
%       From ! {self(), ok},
%       fridge1();
%     {From, {take, Food}} ->
%       From ! {self(), not_found},
%       fridge1();
%     terminate ->
%       ok
%   end.

start(FoodList) ->
  spawn(?MODULE, fridge2, [FoodList]).

fridge2(FoodList) ->
  receive
    {From, {store, Food}} ->
      From !{self(), ok},
      fridge2([Food|FoodList]);
    {From, {take, Food}} ->
      case lists:member(Food, FoodList) of
        true ->
          From ! {self(), {ok, Food}},
          fridge2(lists:delete(Food, FoodList));
        false ->
          From ! {self(), not_found},
          fridge2(FoodList)
      end;
    terminate ->
      ok
  end.

store(Pid, Food) ->
  Pid ! {self(), {store, Food}},
  receive
    {Pid, Msg} -> Msg
  after 3000 -> timeout
  end.

take(Pid, Food) ->
  Pid ! {self(), {take, Food}},
  receive
    {Pid, Msg} -> Msg
  after 3000 -> timeout
  end.


% Fridge 2 attempt
% Good attempt, apparently this is an illegal guard :(
% {From, {take, Food}} when lists:member(Food, FoodList) ->
%   From ! {self(), ok},
%   fridge1(lists:delete(Food, FoodList));
% {From, {take, Food}} ->
%   From ! {self(), not_found},
%   fridge1(FoodList);

