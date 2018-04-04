-module(logger).

-behaviour(gen_server).

-include_lib("stdlib/include/ms_transform.hrl").

-export([start/0, handle_call/3, handle_cast/2, handle_info/2, init/1]).
-export([log/1]).

-define(LOGGER_INPUT_LOGS_ETS, logger_input_logs).
-define(UPLOAD_THRESHOLD_IN_BYTES, 60).
-define(UPLOAD_CHECK_TIME_MS, 500).

start() ->
  gen_server:start({local, ?MODULE}, ?MODULE, [], []).

init(_Args) ->
  ets:new(?LOGGER_INPUT_LOGS_ETS, [public, named_table, ordered_set]),
  start_local_timer(),
  {ok, #{}}.

handle_call(_R, _F, State) -> {reply, ok, State}.
handle_cast(_R, State) -> {noreply, State}.
handle_info(possibly_do_upload, State) ->
  possibly_do_upload(),
  start_local_timer(),
  {noreply, State};
handle_info(_, State) ->
  {noreply, State}.

log(Message) ->
  Order = erlang:unique_integer([monotonic]),
  Field = {Order, Message},
  ets:insert(?LOGGER_INPUT_LOGS_ETS, Field).


start_local_timer() ->
  timer:send_after(?UPLOAD_CHECK_TIME_MS, possibly_do_upload).

should_upload() ->
  Logs = get_previous_logs(),
  case logs_size_in_bytes(Logs) >= ?UPLOAD_THRESHOLD_IN_BYTES of
    true -> {yes, Logs};
    false -> no
  end.

possibly_do_upload() ->
  case should_upload() of
    no -> false;
    {yes, Logs} ->
      io:format("-- Starting upload --~n", []),
      do_upload(Logs),
      lists:foldl(
        fun({Key, Element}, _) ->
            do_delete_locally(Key, Element)
        end, ignored, Logs),
      true
  end.

logs_size_in_bytes(Logs) ->
  lists:foldl(
    fun({_Key, Log}, Acc) ->
        Acc + byte_size(Log)
    end, 0, Logs).

get_previous_logs() ->
  Current = erlang:unique_integer([monotonic]),
  MatchExp =
    ets:fun2ms(fun({Order, Message} = Entry) when Current > Order ->
                   Entry
               end),
  ets:select(?LOGGER_INPUT_LOGS_ETS, MatchExp).

do_upload(Logs) ->
  ListOfLogElements =
    lists:foldl(fun({Key, Element}, Acc) ->
                    BinaryKey = integer_to_binary(Key),
                    [Acc, BinaryKey, <<" -> ">>, Element, <<"\n">>]
                end, [], Logs),
  ToUpload = list_to_binary(ListOfLogElements),
  io:format("do_upload: ~n~s~n", [ToUpload]).

do_delete_locally(Key, _Element) ->
  ets:delete(?LOGGER_INPUT_LOGS_ETS, Key).
