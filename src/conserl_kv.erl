%%% @doc Consul KV API endpoints

-module(conserl_kv).

-export([delete/1, delete/2, delete/3,
         get/1, get/2,
         get_all/1, get_all/2,
         keys/1, keys/2,
         put/2, put/3, put/4,
         watch/1, watch/2, watch/3]).

-type error() :: {error, Reason :: list()}.  %% Returned when an operation fails.

-spec delete(Key :: list()) -> ok | error().
%% @doc Delete the specified key from the Consul KV database.
%%
%% *Example*:
%%
%% <pre lang="erlang">
%% ok = conserl_kv:delete("key").
%% </pre>
%% @end
delete(Key) ->
  delete(Key, false, none).

-spec delete(Key :: list(), Recurse :: boolean()) -> ok | error().
%% @doc Recursively delete all keys under ``Key'' as a prefix when ``Recurse''
%% is ``true'', otherwise delete the key matching the ``Key'' value.
%%
%% *Example*:
%%
%% <pre lang="erlang">
%% ok = conserl_kv:delete("foo/bar/", true).
%% </pre>
%% @end
delete(Key, Recurse) ->
  delete(Key, Recurse, none).

-spec delete(Key :: list(), Recurse :: boolean(), CAS :: integer() | none) -> ok | error().
%% @doc Delete the given ``Key'' using Check-and-Set operations specifying the
%% ``ModifyIndex'' using the ``CAS'' argument, returning ``Result''. If ``Recurse''
%% is set ``true'', delete all keys under ``Key'' as a prefix.
%%
%% *Example*:
%%
%% <pre lang="erlang">
%% Value = conserl_kv:get("maintenance"),
%% ok = conserl_kv:delete("maintenance", false, maps:get(modify_index, Value).
%% </pre>
%% @end
delete(Key, Recurse, CAS) ->
  case gen_server:call(conserl, {delete, [kv, Key], delete_args(Recurse, CAS)}) of
    {error, Reason} -> {error, Reason};
    _ -> ok
  end.

-spec get(Key :: list()) -> {ok, Result :: map()} | error().
%% @doc Get the value and metadata for a given ``Key'' from the KV database.
%%
%% *Example*:
%%
%% <pre lang="erlang">
%% conserl_kv:get("foo").
%% {ok,#{create_index => 557,
%%       flags => 0,
%%       key => "foo",
%%       lock_index => 0,
%%       modify_index => 557,
%%       value => "bar"}}
%% </pre>
%% @end
get(Key) ->
  get(Key, []).

%% @spec get(Key, QArgs) -> Result
%% where
%%       Key    = list()
%%       QArgs  = list()
%%       Result = {ok, map()}|{error, Reason}
%% @doc Return ``Result'' for the given ``Key'' and specified query args (``QArgs'')
%% such as ``@{"dc", "production"@}''.
%% @end
%%
get(Key, QArgs) ->
  case gen_server:call(conserl, {get, [kv, Key], QArgs}) of
    {ok, Payload} ->
      [Result] = build_get_response(Payload),
      {ok, Result};
    {error, Reason} -> {error, Reason}
  end.

%% @spec get_all(Prefix) -> list()
%% where
%%       Prefix    = list()
%%       Result = {ok, list()}|{error, Reason}
%% @doc Return the values for all keys with the supplied ``Prefix''.
%% @end
%%
get_all(Prefix) ->
  get_all(Prefix, []).

%% @spec get_all(Prefix, QArgs) -> list()
%% where
%%       Prefix    = list()
%%       Result = {ok, list()}|{error, Reason}
%% @doc Return the values for all keys with the supplied ``Prefix'' passing in
%% aditional query arguments (``QArgs''), such as ``@{"dc", "production"@}''.
%% @end
%%
get_all(Prefix, QArgs) ->
  Args = lists:append(QArgs, [recurse]),
  case gen_server:call(conserl, {get, [kv, Prefix], Args}) of
    {ok, Response} ->
      {ok, build_get_response(Response)};
    {error, Reason} -> {error, Reason}
  end.

%% @spec keys(Prefix) -> Result
%% where
%%       Prefix = list()
%%       Result = {ok, list()}|{error, Reason}
%% @doc List all keys under the given ``Prefix''.
%% @end
%%
keys(Prefix) ->
  keys(Prefix, []).

%% @spec keys(Prefix, QArgs) -> Result
%% where
%%       Prefix = list()
%%       QArgs  = list()
%%       Result = {ok, list()}|{error, Reason}
%% @doc List keys for the prefix. To add a separator for limiting the keys
%% returned, pass ``@{separator, Value@}'' in the ``QArgs"" %% such
%% as ``@{"dc", "production"@}''.
%% @end
%%
keys(Prefix, QArgs) ->
  Args = lists:append(QArgs, [keys]),
  case gen_server:call(conserl, {get, [kv, Prefix], Args}) of
    {ok, Result} -> {ok, [binary_to_list(K) || K <- Result]};
    {error, Reason} -> {error, Reason}
  end.

%% @spec put(Key, Value) -> Result
%% where
%%       Key    = list()
%%       Value  = list()
%%       Result = boolean()|{error, Reason}
%% @doc Store ``Value'' for ``Key'' returning ``Result''.
%% @end
%%
put(Key, Value) ->
  put(Key, Value, 0, none).

-spec put(Key :: list(), Value :: list(), Flags :: integer()) -> boolean() | {error, list()}.
%% @doc Store ``Value'' while specifying ``Flags'' for ``Key'' returning ``Result''.
%% @end
%%
put(Key, Value, Flags) ->
  put(Key, Value, Flags, none).

%% @spec put(Key, Value, Flags, CAS) -> Result
%% where
%%       Key    = list()
%%       Value  = list()
%%       Flags  = integer()
%%       CAS    = integer()
%%       Result = boolean()|{error, Reason}
%% @doc Store ``Value'' while specifying ``Flags'' for ``Key'', using the
%% Check-and-Set operation, specifying the ``ModifyIndex'' value as ``CAS''
%% returning ``Result''.
%% @end
%%
put(Key, Value, Flags, CAS) ->
  Args = case CAS of
    none -> [{flags, Flags}];
    _ -> [{flags, Flags}, {cas, CAS}]
  end,
  case gen_server:call(conserl, {put, [kv, Key], Value, Args}) of
    {error, Reason} -> {error, Reason};
    Response -> list_to_atom(Response)
  end.

%% @spec watch(Key) -> Result
%% where
%%       Key    = list()
%%       Result = {ok, map()}|{error, Reason}
%% @doc Blocking watch on the specified ``Key'', returning ``Result''.
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
%% where
%%       Key    = list()
%%       Result = {ref(), map()}|{ref(), {error, Reason}}
%% @doc Asynchonous watch on the specified ``Key'', calling the callback ``Fun''
%% with the ``Result''.
%% @end
%%
watch(Key, Fun) ->
  watch(Key, [], Fun).

%% @spec watch(Key, QArgs, Fun) -> {ok, ref()}
%% where
%%       Key    = list()
%%       QArgs  = list()
%%       Result = {ref(), map()}|{ref(), {error, Reason}}
%% @doc Asynchonous watch on the specified key, calling the callback 'Fun'
%% with the results of the call as Result, passing in aditional query
%% arguments (``QArgs''), such as ``@{"dc", "production"@}''.
%% @end
%%
watch(Key, QArgs, Fun) ->
  Reply = fun(Response) ->
    case Response of
      {error, Reason} -> Fun({error, Reason});
      {Ref, Payload} ->
        [Result] = build_get_response(Payload),
        Fun({Ref, Result})
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
-spec delete_args(boolean(), none | integer()) -> list().
%% @doc Return a list of request args for a delete
delete_args(false, none) -> [];
delete_args(true, none)  -> [recurse];
delete_args(false, CAS)  -> [{cas, CAS}];
delete_args(true, CAS)   -> [recurse, {cas, CAS}].

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
%% where
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
