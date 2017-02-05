-module(recursion).
-export([fac/1, len/1, tail_fac/1, tail_len/1, duplicate/2, tail_dup/2, reverse/1, tail_reverse/1, sublist/2, zip/2, quicksort/1, lc_quicksort/1]).

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


% To push things a bit further, we'll write a zipping function. A zipping function will take two lists of same length as parameters and will join them as a list of tuples which all hold two terms. Our own zip/2 function will behave this way:
%1> recursive:zip([a,b,c],[1,2,3]).
%[{a,1},{b,2},{c,3}]

zip(L1, L2) -> tail_reverse(tail_zip(L1, L2, [])).

tail_zip([], [], Acc) -> Acc;
tail_zip([], _, Acc) -> Acc;
tail_zip(_, [], Acc) -> Acc;
tail_zip([H1|T1], [H2|T2], Acc) ->
  tail_zip(T1, T2, [{H1, H2}|Acc]).

% I can (and will) now assume recursion and tail recursion make sense to you, but just to make sure, I'm going to push for a more complex example, quicksort. Yes, the traditional "hey look I can write short functional code" canonical example. A naive implementation of quicksort works by taking the first element of a list, the pivot, and then putting all the elements smaller or equal to the pivot in a new list, and all those larger in another list. We then take each of these lists and do the same thing on them until each list gets smaller and smaller. This goes on until you have nothing but an empty list to sort, which will be our base case. This implementation is said to be naive because smarter versions of quicksort will try to pick optimal pivots to be faster. We don't really care about that for our example though.

% We will need two functions for this one: a first function to partition the list into smaller and larger parts and a second function to apply the partition function on each of the new lists and to glue them together. First of all, we'll write the glue function:

% GLUE FN
% quicksort([]) -> [];
% quicksort([Pivot|Rest]) ->
%   {Smaller, Larger} = partition(Pivot, Rest, [], []),
%   quicksort(Smaller) ++ [Pivot] ++ quicksort(Larger).
% 
% % PARTITION FN
% partition(_, [], Smaller, Larger) ->
%   {Smaller, Larger};
% partition(Pivot, [H|T], Smaller, Larger) ->
%   if H =< Pivot -> partition(Pivot, T, [H|Smaller], Larger);
%      H > Pivot -> partition(Pivot, T, Smaller, [H|Larger])
%   end.
% 


















% FROM SCRATCH
% How does quicksort work here?
% We take in a list as input
% We generate two lists, one of which is for smaller elements, one of which is for larger
% We then glue them together smaller (sorted) + pivot + larger

quicksort([]) -> [];
quicksort([Pivot|Remainder]) ->
  {Smaller, Larger} = partition(Pivot, Remainder, [], []),
  quicksort(Smaller) ++ [Pivot] ++ quicksort(Larger).

% How do we partition? We use tail recursion
% We're given an array (remainder) that we need to allocate into the smaller and larger arrays
partition(_, [], Smaller, Larger) -> {Smaller, Larger};
partition(Pivot, [H|T], Smaller, Larger) ->
  if H > Pivot ->
       partition(Pivot, T, Smaller, [H|Larger]);
     H =< Pivot ->
       partition(Pivot, T, [H|Smaller], Larger)
  end.

% LIST COMPREHENSION VERSION
% That's pretty cool actually
lc_quicksort([]) -> [];
lc_quicksort([Pivot|Rest]) ->
  lc_quicksort([Smaller || Smaller <- Rest, Smaller < Pivot]) ++ [Pivot] ++ lc_quicksort([Larger || Larger <- Rest, Larger > Pivot]).


