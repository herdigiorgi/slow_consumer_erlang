-module(logger).

-export([log/1]).

log(Message) ->
  Order = erlang:unique_integer([monotonic]),
  Field = {Order, Message},
  ets:insert(log_uploader, Field),
  case log_uploader:check_if_should_upload() of
    {yes, _} -> gen_server:cast(log_uploader, do_upload);
    _ -> ok
  end.

