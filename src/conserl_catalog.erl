%%% @author Gavin M. Roy <gavinmroy@gmail.com>
%%% @doc Consul Catalog API endpoints

-module(conserl_catalog).

-export([nodes/0, nodes/1, services/0, services/1]).

nodes() ->
  conserl_catalog:nodes(none).

nodes(DC) ->
  Args = case DC of
    none -> [];
    _ -> [{dc, DC}]
  end,
  case gen_server:call(conserl, {get, [catalog, nodes], Args}) of
    {ok, Result} -> {ok, build_node_list(Result)};
    Other -> Other
  end.

services() ->
  services(none).

services(DC) ->
  Args = case DC of
    none -> [];
    _ -> [{dc, DC}]
  end,
  case gen_server:call(conserl, {get, [catalog, services], Args}) of
    {ok, Result} -> {ok, build_service_list(Result)};
    Other -> Other
  end.


build_node_list(Nodes) -> build_node_list(Nodes, []).
build_node_list([], Acc) -> Acc;
build_node_list([H|T], Acc) ->
  build_node_list(T, lists:append(Acc,
                  [{binary_to_list(proplists:get_value(<<"Node">>, H)),
                    binary_to_list(proplists:get_value(<<"Address">>, H))}])).

build_service_list(Nodes) -> build_service_list(Nodes, []).
build_service_list([], Acc) -> Acc;
build_service_list([{K, V}|T], Acc) ->
  build_service_list(T,
                     lists:append(Acc,
                                  [{binary_to_list(K), [binary_to_list(N) || N <- V]}])).
