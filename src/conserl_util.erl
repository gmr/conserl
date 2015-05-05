%% @hidden

-module(conserl_util).

%% API
-export([get/3,
         build_url/4,
         build_path/1,
         build_query/1,
         build_full_path/2]).

-include("conserl.hrl").

get(#state{host=Host, port=Port, acl=ACL}, Path, QArgs) when ACL =/= undefined ->
  get(Host, Port, Path, lists:merge(QArgs, [{acl, ACL}]));

get(State, Path, QArgs) ->
  get(State#state.host, State#state.port, Path, QArgs).

get(Host, Port, Path, QArgs) ->
  URL = build_url(Host, Port, Path, QArgs),
  case httpc:request(URL) of
    {ok, {{_Vsn, 200, _Reason}, _Headers, Body}} ->
      Decoded = jsx:decode(list_to_binary(Body)),
      lager:info("Body: ~p", [Decoded]),
      {ok, Decoded};
    {error, Reason} ->
      lager:error("Error querying Consul: ~p", [Reason]),
      {error, Reason}
  end.

build_url(Host, Port, Path, QArgs) ->
  string:join(["http://", Host, ":", integer_to_list(Port), build_full_path(Path, QArgs)], "").

build_full_path(Path, []) ->
  string:join(["/", ?API_VERSION, build_path(Path)], "");

build_full_path(Path, QArgs) ->
  string:join(["/", ?API_VERSION, build_path(Path), "?", build_query(QArgs)], "").

build_path(Args) ->
  build_path(Args, []).

build_path([Part|Parts], Path) when is_atom(Part) =:= true ->
  build_path(Parts, string:join([Path, http_uri:encode(atom_to_list(Part))], "/"));

build_path([Part|Parts], Path) ->
  build_path(Parts, string:join([Path, http_uri:encode(Part)], "/"));

build_path([], Path) -> Path.

build_query(Args) ->
  build_query(Args, []).

build_query([{Key,Value}|Args], Parts) when is_atom(Key) =:= true ->
  build_query(Args, lists:merge(Parts, [string:join([http_uri:encode(atom_to_list(Key)),
                                                     encode_query_value(Value)], "=")]));

build_query([{Key,Value}|Args], Parts) ->
  build_query(Args, lists:merge(Parts, [string:join([http_uri:encode(Key),
                                                     encode_query_value(Value)], "=")]));

build_query([Key|Args], Parts) when is_atom(Key) =:= true ->
  build_query(Args, lists:merge(Parts, [http_uri:encode(atom_to_list(Key))]));

build_query([Key|Args], Parts) ->
  build_query(Args, lists:merge(Parts, [http_uri:encode(Key)]));

build_query([], Parts) ->
  string:join(lists:sort(Parts), "&").

encode_query_value(Value) when is_atom(Value) =:= true ->
  http_uri:encode(atom_to_list(Value));

encode_query_value(Value) when is_list(Value) =:= true ->
  http_uri:encode(Value);

encode_query_value(Value) when is_integer(Value) =:= true ->
  integer_to_list(Value).
