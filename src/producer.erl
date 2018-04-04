-module(producer).

-export([produce/1]).

produce(N) when N =< 0 -> ok;
produce(N) ->
  spawn_link(fun log_something/0),
  produce(N - 1).

log_something() ->
  Message = create_message_with_time(),
  logger:log(Message).

create_message_with_time() ->
  TimeBinary = integer_to_binary(os:system_time()),
  <<<<"Message: ">>/binary, TimeBinary/binary>>.
