-module(hhfuns).
-compile(export_all).

one() -> 1.
two() -> 2.

add(X, Y) -> X() + Y().

% So what are the gains of using functions in that manner? Well a little example might be needed in order to understand it. We'll add a few functions to hhfuns that work recursively over a list to add or subtract one from each integer of a list:

% Tail recursive version
% increment(L) ->
%   lists:reverse(tail_increment(L, [])).
% tail_increment([], Acc) -> Acc;
% tail_increment([H|T], Acc) ->
%   tail_increment(T, [H+1|Acc]).

% Non tail optimized versions
increment([]) -> [];
increment([H|T]) -> [H+1|increment(T)].

decrement([]) -> [];
decrement([H|T]) -> [H-1|decrement(T)].

% See how similar these functions are? They basically do the same thing: they cycle through a list, apply a function on each element (+ or -) and then call themselves again. There is almost nothing changing in that code: only the applied function and the recursive call are different. The core of a recursive call on a list like that is always the same. We'll abstract all the similar parts in a single function (map/2) that will take another function as an argument:

map([], _) -> [];
map([H|T], F) ->
  [F(H)|map(T, F)].

incr(X) -> X+1.
decr(X) -> X-1.

%% only keep even numbers
% List comprehension:
% even(L) -> 
%   [X || X <- L, X rem 2 == 0].

% Tail recursion
even(L) -> lists:reverse(even(L,[])).
even([], Acc) -> Acc;
even([H|T], Acc) when H rem 2 == 0 ->
  even(T, [H|Acc]);
even([_|T], Acc) ->
  even(T, Acc).

%% only keep men older than 60
% How do we guard this correctly? 
% We are not when-ing that the person is correclty a person
% Instead we are when-ing that the person is old enough
% And we are pattern matching that the person is correctly a person
old_men(L) -> lists:reverse(old_men(L,[])).
old_men([], Acc) -> Acc;
old_men([Person = {male, Age}|People], Acc) when Age > 60 ->
  old_men(People, [Person|Acc]);
old_men([_|People], Acc) ->
  old_men(People, Acc).
  
filter(P, L) -> lists:reverse(filter(P, L, [])).
filter(_, [], Acc) -> Acc;
filter(P, [H|T], Acc) ->
  case P(H) of
    true -> filter(P, T, [H|Acc]);
    false -> filter(P, T, Acc)
  end.
% ATTEMPT 2: how to do it with ifs
% NOTE: you ALSO cannot do ifs! They are just guards. And you cannot execute a function in a guard. (It's just pattern matching)
% filter(P, [H|T], Acc) ->
%   if P(H) == true ->
%       filter(P, T, [H|Acc]);
%     true -> filter(P,T,Acc)
%   end.
% ATTEMPT 0: illegal guard expression (sadness)
% filter(P, [H|T], Acc) when P(H) == true ->
%   filter(P, T, [H|Acc]);
% filter(P, [_|T], Acc) ->
%   filter(P, T, Acc).

% In the previous chapter, another kind of recursive manipulation we applied on lists was to look at every element of a list one after the other and reduce them to a single answer. This is called a fold and can be used on the following functions:
% To find how the fold should behave, we've got to find all the common points of these actions and then what is different. As mentioned above, we always have a reduction from a list to a single value. Consequently, our fold should only consider iterating while keeping a single item, no list-building needed. Then we need to ignore the guards, because they're not always there: these need to be in the user's function. In this regard, our folding function will probably look a lot like sum.
% A subtle element of all three functions that wasn't mentioned yet is that every function needs to have an initial value to start counting with. In the case of sum/2, we use 0 as we're doing addition and given X = X + 0, the value is neutral and we can't mess up the calculation by starting there. If we were doing multiplication we'd use 1 given X = X * 1. The functions min/1 and max/1 can't have a default starting value: if the list was only negative numbers and we started at 0, the answer would be wrong. As such, we need to use the first element of the list as a starting point. Sadly, we can't always decide this way, so we'll leave that decision to the programmer. By taking all these elements, we can build the following abstraction:
% fold(F, First, Rest) ->
%   ok.
fold(_, Reduced, []) -> Reduced;
fold(F, Reduced, [H|T]) ->
  fold(F, F(H, Reduced), T).

reverse(L) -> fold(fun(H, Reduced) -> [H|Reduced] end, [], L).
map2(F, L) -> reverse(fold(fun(H, Reduced) -> [F(H)|Reduced] end, [], L)).
filter2(P, L) -> 
  reverse(fold(fun(H, Reduced) -> 
        case P(H) of
          true -> [H|Reduced];
          false -> Reduced
        end
    end, [], L)).
% CLEANER VERSION OF ^
filter3(P, L) ->
  F = fun(H, Reduced) ->
      case P(H) of
        true -> [H|Reduced];
        false -> Reduced
      end
  end,
  reverse(fold(F, [], L)).




