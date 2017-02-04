-module(append).
-compile(export_all).

% append(Element, List) -> 
%   %[List | Element].
%   List ++ [Element].
% 

% If we send in an empty set (list) and a term X to be added, it returns us a list containing only X. Otherwise, the function lists:member/2 checks whether an element is part of a list and returns true if it is, false if it is not. In the case we already had the element X in the set, we do not need to modify the list. Otherwise, we add X as the list's first element.

% Fall-thru case (empty set)
insert(X, []) ->
  [X];

insert(X, Set) ->
  case lists:member(X, Set) of
    true -> Set;
    false -> [X|Set]
  end.


