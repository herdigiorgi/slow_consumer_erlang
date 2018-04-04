-module(slow_consumer_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_, _) ->
  log_uploader:start().

stop(_) -> ok.
