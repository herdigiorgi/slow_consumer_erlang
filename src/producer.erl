-module(producer).

-export([produce/1]).

produce(N) when N =< 0 -> ok;
produce(N) ->
  spawn_link(fun log_something/0),
  produce(N - 1).

log_something() ->
  TestingOrder = erlang:unique_integer([monotonic]),
  TestingOrderBinary = integer_to_binary(TestingOrder),
  Message = << <<"Message: ">>/binary, TestingOrderBinary/binary>>,
  logger:log(Message).
