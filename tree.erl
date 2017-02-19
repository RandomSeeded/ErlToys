-module(tree).
-export([empty/0, insert/3, lookup/2, has_val/2]).

% To represent nodes, tuples are an appropriate data structure. For our implementation, we can then define these tuples as {node, {Key, Value, Smaller, Larger}} (a tagged tuple!), where Smaller and Larger can be another similar node or an empty node ({node, nil}). We won't actually need a concept more complex than that.

empty() ->
  {node, 'nil'}.

insert(Key, Value, {node, 'nil'}) -> 
  { node, { Key, Value, { node, 'nil' }, { node, 'nil' } } };
insert(NewKey, NewValue, {node, { Key, Value, Smaller, Larger }}) when NewKey > Key ->
  % what does this need to look like? It needs to return the whole tree
  % But with larger modified
  % insert(NewKey, NewValue, Larger);
  { node, { Key, Value, Smaller, insert(NewKey, NewValue, Larger) } };
insert(NewKey, NewValue, {node, { Key, Value, Smaller, Larger }}) when NewKey < Key ->
  { node, { Key, Value, insert(NewKey, NewValue, Smaller), Larger } }.

% ORIGINAL
% lookup(_, {node, 'nil'}) ->
%   false;
% lookup(Lookup, { node, { Key, Value, Smaller, Larger } }) ->
%   if Lookup == Key ->
%        Value;
%      Lookup > Key ->
%        lookup(Lookup, Larger);
%      Lookup < Key ->
%        lookup(Lookup, Smaller)
%   end.

% IMPROVED: this allows us to distinguish between value undef'd and undefd
% lookup(_, {node, 'nil'}) ->
%   undefined;
% lookup(Lookup, { node, { Key, Value, Smaller, Larger } }) ->
%   if Lookup == Key ->
%        {ok, Value};
%      Lookup > Key ->
%        lookup(Lookup, Larger);
%      Lookup < Key ->
%        lookup(Lookup, Smaller)
%   end.

% MORE ERLANG'D (no if)
lookup(_, {node, 'nil'}) ->
  undefined;
lookup(Lookup, { node, { Key, Value, _, _ } }) when Lookup == Key ->
  {ok, Value};
lookup(Lookup, { node, { Key, _, Smaller, _ } }) when Lookup < Key ->
  lookup(Lookup, Smaller);
lookup(Lookup, { node, { _, _, _, Larger } }) ->
  lookup(Lookup, Larger).

%% looks for a given value 'Val' in the tree.
% (exception practice)
has_val(Target, Node) ->
  try has_value(Target, Node) of
    false -> false
  catch
    true -> true
  end.

has_value(_, { node, 'nil' }) ->
  false;
has_value(Target, { node, { _, Value, _, _ } }) when Target == Value ->
  throw(true);
has_value(Target, { node, { _, _, Smaller, Larger } }) ->
  % has_value(Target, Smaller) or has_value(Target, Larger).
  % MORE ERLANG-Y WAY (why?) It's completely equivalent really
  case has_value(Target, Smaller) of
    true -> true;
    false -> has_value(Target, Larger)
  end.
