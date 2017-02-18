-module(calc).
-export([rpn/1,rpn_test/0]).

rpn(L) when is_list(L) ->
  [Res] = lists:foldl(fun rpn/2, [], string:tokens(L, " ")),
  Res.

% Our function is invoked with two parameters: the element of the expression and the stack

% when we get regular numbers, all we do is return the number tacked onto the stack
% However, when we get operators, we need to take the top two values off the stack and do things with em
rpn("+", [Top|[Second|Rest]]) ->
  [Second+Top|Rest];
rpn("*", [Top|[Second|Rest]]) ->
  [Second*Top|Rest];
rpn("-", [Top|[Second|Rest]]) ->
  [Second-Top|Rest];
rpn("/", [Top|[Second|Rest]]) ->
  [Second/Top|Rest];
rpn(X, Stack) -> [read(X)|Stack].


% converts string to either float or int
read(N) ->
  case string:to_float(N) of
    {error,no_float} -> list_to_integer(N);
    {F,_} -> F
  end.

rpn_test() ->
  5 = rpn("2 3 +"),
  87 = rpn("90 3 -"),
  -4 = rpn("10 4 3 + 2 * -"),
  -2.0 = rpn("10 4 3 + 2 * - 2 /"),
  ok = try
    rpn("90 34 12 33 55 66 + * - +")
  catch
    error:{badmatch,[_|_]} -> ok
  end,
  ok.

