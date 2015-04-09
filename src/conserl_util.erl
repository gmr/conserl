%% @hidden

-module(conserl_util).

%% API
-export([get/3,
         build_path/1,
         build_query/1,
         build_full_path/2]).

-include("conserl.hrl").

get(#state{host=Host, port=Port, acl=ACL}, Path, QArgs) when ACL =/= undefined ->
  get(Host, Port, Path, lists:merge(QArgs, [{acl, ACL}]));

get(State, Path, QArgs) ->
  get(State#state.host, State#state.port, Path, QArgs).

get(Host, Port, Path, QArgs) ->
  {ok, Conn} = shotgun:open(Host, Port),
  FullPath = build_full_path(Path, QArgs),
  {ok, Response} = shotgun:get(Conn, FullPath),
  shotgun:close(Conn),
  Response.

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
                                                     http_uri:encode(Value)], "=")]));

build_query([{Key,Value}|Args], Parts) ->
  build_query(Args, lists:merge(Parts, [string:join([http_uri:encode(Key),
                                                     http_uri:encode(Value)], "=")]));

build_query([Key|Args], Parts) when is_atom(Key) =:= true ->
  build_query(Args, lists:merge(Parts, [http_uri:encode(atom_to_list(Key))]));

build_query([Key|Args], Parts) ->
  build_query(Args, lists:merge(Parts, [http_uri:encode(Key)]));

build_query([], Parts) ->
  string:join(Parts, "&").
