-module(recursion).
-export([fac/1, len/1, tail_fac/1, tail_len/1, duplicate/2, tail_dup/2, reverse/1, tail_reverse/1, sublist/2]).

fac(N) when N == 1 -> N;
fac(N) when N >= 1 -> N * fac(N-1).


% Thoughts on recursively getting length of a list:
% We use pattern matching
% We match against what: a head and a list? vs just a head?

len([_|T]) -> 
  len(T) + 1;

len([]) ->
  0.

tail_fac(N) -> tail_fac(N,1).

tail_fac(0, Acc) -> Acc;
tail_fac(N, Acc) when N > 0 -> tail_fac(N-1, N*Acc).

% how can we tail recurse our length function?
% We used to do 1 + remaining length
% Now we instead want to do length + accumulator

% tail_len([]) -> 0;
% tail_len([H|T]) ->
%   tail_len([H|T], 0).
% 
% tail_len([], Acc) -> 
%   Acc;
% 
% tail_len([H|T], Acc) -> 
%   tail_len(T, Acc+1).

% IMPROVED? Note: my way is safer (demands a list). Dunno if that's better or not
tail_len(L) -> tail_len(L, 0).
tail_len([], Acc) -> Acc;
tail_len([_|T], Acc) ->
  tail_len(T, Acc+1).

% The first function we'll write will be duplicate/2. This function takes an integer as its first parameter and then any other term as its second parameter. It will then create a list of as many copies of the term as specified by the integer. Like before, thinking of the base case first is what might help you get going. For duplicate/2, asking to repeat something 0 time is the most basic thing that can be done. All we have to do is return an empty list, no matter what the term is. Every other case needs to try and get to the base case by calling the function itself. We will also forbid negative values for the integer, because you can't duplicate something -n times:

% RAW ATTEMPT 0
% duplicate(0, Element) -> [];
% duplicate(Times, Element) ->
%   duplicate(Times-1, Element) ++ [Element].

% IMPROVED. Thing to note here: you couldn't do dupe | element, but you can do element | dupe
duplicate(0, _) -> [];
duplicate(Times, Element) ->
  [Element|duplicate(Times-1, Element)].

% TAIL RECURSION
% What will you accumulate? You can accumulate the thing you're building as you go along
tail_dup(Times, Element) -> tail_dup(Times, Element, []).

tail_dup(0, _, Acc) -> Acc;
tail_dup(Times, Element, Acc) when Times > 0 -> % note: this should be guarded!! Make sure we dont have overlap for when n == 0
  tail_dup(Times-1, Element, [Element|Acc]).


% REVERSE/1: reverses a list

% raw recursion
reverse([]) -> [];
reverse([H|T]) ->
  reverse(T) ++ [H].

tail_reverse(L) -> tail_reverse(L, []).

tail_reverse([], Acc) -> Acc;
tail_reverse([H|T], Acc) ->
  tail_reverse(T, [H|Acc]).

% Another function to implement could be sublist/2, which takes a list L and an integer N, and returns the N first elements of the list. As an example, sublist([1,2,3,4,5,6],3) would return [1,2,3].

% REVERSE THE LIST AT THE END
% (better than doing shitty list accumulations)
sublist(L, N) -> tail_reverse(sublist(L, N, [])).

sublist(_, 0, Acc) -> Acc;
sublist([], _, Acc) -> Acc;
sublist([H|T], N, Acc) when N > 0 -> 
  sublist(T, N-1, [H|Acc]).


















