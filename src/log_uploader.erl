-module(log_uploader).

-behaviour(gen_server).

-include_lib("stdlib/include/ms_transform.hrl").

-define(UPLOAD_THRESHOLD_IN_BYTES, 60).

-export([start/0, handle_call/3, handle_cast/2, init/1]).
-export([check_if_should_upload/0]).

start() ->
  gen_server:start({local, ?MODULE}, ?MODULE, [], []).

init(_Args) ->
  ets:new(?MODULE, [public, named_table, ordered_set]),
  {ok, #{}}.

handle_call(_R, _F, State) -> {reply, ok, State}.
handle_cast(do_upload, State) ->
  possibly_do_upload(),
  {noreply, State};
handle_cast(_R, State) -> {noreply, State}.

check_if_should_upload() ->
  Logs = get_previous_logs(),
  case logs_size_in_bytes(Logs) >= ?UPLOAD_THRESHOLD_IN_BYTES of
    true -> {yes, Logs};
    false -> no
  end.

possibly_do_upload() ->
  case check_if_should_upload() of
    no -> false;
    {yes, Logs} ->
      do_upload(Logs),
      do_delete_locally(Logs)
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
  ets:select(?MODULE, MatchExp).

do_upload(Logs) ->
  ListOfLogElements =
    lists:foldl(fun({Key, Element}, Acc) ->
                    BinaryKey = integer_to_binary(Key),
                    [Acc, BinaryKey, <<" -> ">>, Element, <<"\n">>]
                end, [], Logs),
  ToUpload = list_to_binary(ListOfLogElements),
  io:format("do_upload: ~n~s~n", [ToUpload]).

do_delete_locally(Logs) ->
  lists:foldl(
    fun({Key, _Element}, _) ->
        ets:delete(?MODULE, Key)
    end, ignored, Logs).
