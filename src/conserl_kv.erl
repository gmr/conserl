%%% @author Gavin M. Roy <gavinmroy@gmail.com>
%%% @copyright 2015, Gavin M. Roy
%%% @doc Consul KV API endpoints

-module(conserl_kv).

-export([get/1, get/2,
         get_all/1, get_all/2,
         keys/1, keys/2,
         watch/1, watch/2, watch/3]).

%% @spec get(Key) -> map()
%%       Prefix    = list()
%%       Result = {ok, list()}|{error, Reason}
%% @doc Return a map() value for the given key.
%% @end
%%
get(Key) ->
  get(Key, []).

get(Key, QArgs) ->
  case gen_server:call(conserl, {get, [kv, Key], QArgs}) of
    {ok, Payload} ->
      [Result] = build_get_response(Payload),
      {ok, Result};
    {error, Reason} -> {error, Reason}
  end.

%% @spec get_all(Prefix) -> list()
%%       Prefix    = list()
%%       Result = {ok, list()}|{error, Reason}
%% @doc Return the values for all keys with the supplied prefix.
%% @end
%%
get_all(Prefix) ->
  get_all(Prefix, []).

%% @spec get_all(Prefix, QArgs) -> list()
%%       Prefix    = list()
%%       Result = {ok, list()}|{error, Reason}
%% @doc Return the values for all keys with the supplied prefix passing in
%% aditional query arguments, such as 'dc'.
%% @end
%%
get_all(Prefix, QArgs) ->
  Args = lists:append(QArgs, [recurse]),
  lager:info("Args: ~p", [Args]),
  case gen_server:call(conserl, {get, [kv, Prefix], Args}) of
    {ok, Payload} ->
      {ok, build_get_response(Payload)};
    {error, Reason} -> {error, Reason}
  end.

%% @spec keys(Prefix) -> Result
%%       Prefix = list()
%%       Result = {ok, list()}|{error, Reason}
%% @doc List all keys under the given prefix
%% @end
%%
keys(Prefix) ->
  keys(Prefix, []).

%% @spec keys(Prefix, QArgs) -> Result
%%       Prefix = list()
%%       QArgs  = list()
%%       Result = {ok, list()}|{error, Reason}
%% @doc List keys for the prefix. To add a separator for limiting the keys
%% returned, pass '{separator, Value}' in the QArgs.
%% @end
%%
keys(Prefix, QArgs) ->
  Args = lists:append(QArgs, [keys]),
  case gen_server:call(conserl, {get, [kv, Prefix], Args}) of
    {ok, Result} -> {ok, [binary_to_list(K) || K <- Result]};
    {error, Reason} -> {error, Reason}
  end.


%% @spec watch(Key) -> Result
%%       Prefix    = list()
%%       Result = {ok, map()}|{error, Reason}
%% @doc Blocking watch on the specified key
%% @end
%%
watch(Key) ->
  case get(Key, []) of
    {ok, Response} ->
      case gen_server:call(conserl,
                           {get, [kv, Key],
                           [{index, maps:get(modify_index, Response)}],
                           {timeout, infinity}}, infinity) of
        {ok, Payload} ->
          [Result] = build_get_response(Payload),
          {ok, Result};
        {error, Reason} -> {error, Reason}
      end;
    {error, Reason} -> {error, Reason}
  end.

%% @spec watch(Key, Fun) -> {ok, ref()}
%%       Prefix = list()
%%       Result = {ref(), map()}|{ref(), {error, Reason}}
%% @doc Asynchonous watch on the specified key, calling the callback 'Fun'
%% with the results of the call as Result.
%% @end
%%
watch(Key, Fun) ->
  watch(Key, [], Fun).

%% @spec watch(Key, QArgs, Fun) -> {ok, ref()}
%%       Prefix = list()
%%       QArgs  = list()
%%       Result = {ref(), map()}|{ref(), {error, Reason}}
%% @doc Asynchonous watch on the specified key, calling the callback 'Fun'
%% with the results of the call as Result, passing in aditional query
%% arguments, such as 'dc'.
%% @end
%%
watch(Key, QArgs, Fun) ->
  Reply = fun(Response) ->
    case Response of
      {ok, Payload} ->
        [Result] = build_get_response(Payload),
        Fun({ok, Result});
      {error, Reason} -> Fun({error, Reason})
    end
  end,
  case get(Key, QArgs) of
    {ok, Response} ->
      Args = lists:append(QArgs, [{index, maps:get(modify_index, Response)}]),
      case gen_server:call(conserl, {get, [kv, Key], Args, Reply}) of
        {ok, Ref} -> {ok, Ref};
        {error, Response} -> Fun({error, Response})
      end;
    {error, Response} -> Fun({error, Response})
  end.


%% @private
%% @spec build_get_response(list()) -> list()
%% @doc Transform the list GET response of entries to maps from proplists.
%% @end
%%
build_get_response(Entries) ->
  build_get_response(Entries, []).

%% @private
%% @spec build_get_response(list(), list()) -> list()
%% @doc Transform the list GET response of entries to maps from proplists.
%% @end
%%
build_get_response([], Acc) -> Acc;
build_get_response([H|T], Acc) ->
  build_get_response(T, lists:append(Acc, [build_key_map(H)])).

%% @private
%% @spec build_key_map(Payload) -> map()
%%       Payload   = list()
%% @doc Build the response to a kv GET as a map, base64 decoding the value
%% @end
%%
build_key_map(Payload) ->
  Value = proplists:get_value(<<"Value">>, Payload),
  #{create_index => proplists:get_value(<<"CreateIndex">>, Payload),
    modify_index => proplists:get_value(<<"ModifyIndex">>, Payload),
    lock_index => proplists:get_value(<<"LockIndex">>, Payload),
    key => binary_to_list(proplists:get_value(<<"Key">>, Payload)),
    flags => proplists:get_value(<<"Flags">>, Payload),
    value => base64:decode_to_string(binary_to_list(Value))}.
