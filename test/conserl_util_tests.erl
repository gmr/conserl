-module(conserl_util_tests).

-include("conserl.hrl").

-include_lib("eunit/include/eunit.hrl").

-export([test_build_path/0,
         test_build_query/0,
         test_build_full_path/0,
         test_build_full_path_with_qargs/0,
         test_build_url/0]).

awerl_test_() ->
  {inparallel, [
    {"test_build_path", ?MODULE, test_build_path},
    {"test_build_query", ?MODULE, test_build_query},
    {"test_build_full_path", ?MODULE, test_build_full_path},
    {"test_build_full_path_with_qargs", ?MODULE, test_build_full_path_with_qargs},
    {"test_build_url", ?MODULE, test_build_url}
  ]}.

split_query(Query) ->
    case re:split(string:strip(Query, left, $?), "&", [{return, list}]) of
        [""]    -> [];
        QParams -> [split_uri(Param, "=", Param) || Param <- QParams]
    end.

split_uri(UriPart, SplitChar, NoMatchResult) ->
    split_uri(UriPart, SplitChar, NoMatchResult, 1, 1).

split_uri(UriPart, SplitChar, NoMatchResult, SkipLeft, SkipRight) ->
    case re:run(UriPart, SplitChar) of
	{match, [{Match, _}]} ->
	    {string:substr(UriPart, 1, Match + 1 - SkipLeft),
	     string:substr(UriPart, Match + 1 + SkipRight, length(UriPart))};
	nomatch ->
	    NoMatchResult
    end.

test_build_path() ->
  ?assertEqual("/foo/b%40r/baz", conserl_util:build_path(["foo", "b@r", "baz"])).

test_build_query() ->
  ?assertEqual("dc=pr%40duction&recurse",
               conserl_util:build_query([{"dc", "pr@duction"}, "recurse"])).

test_build_full_path() ->
  Path = conserl_util:build_full_path(["kv", "keyN@me"], []),
  ?assertEqual("/v1/kv/keyN%40me", Path).

test_build_full_path_with_qargs() ->
  Path = conserl_util:build_full_path(["kv", "keyN@me"],
                                      [{"dc", "foo"}, "recurse", "consistent"]),
  [BasePath, Query] = string:tokens(Path, "?"),
  ?assertEqual("/v1/kv/keyN%40me", BasePath),
  QArgs = split_query(Query),
  ?assert(lists:member("recurse", QArgs)),
  ?assert(lists:member("consistent", QArgs)),
  ?assertEqual("foo", proplists:get_value("dc", QArgs)).

test_build_url() ->
  URL = conserl_util:build_url("localhost", 8500,
                               ["kv", "keyN@me"],
                               [{"dc", "foo"}, "recurse", "consistent"]),
  case http_uri:parse(URL) of
    {ok, {Scheme, _UserInfo, Host, Port, Path, Query}} ->
      ?assertEqual(http, Scheme),
      ?assertEqual("localhost", Host),
      ?assertEqual(8500, Port),
      ?assertEqual("/v1/kv/keyN%40me", Path),
      QArgs = split_query(Query),
      io:format("QArgs: ~p~n", [QArgs]),
      ?assert(lists:member("recurse", QArgs)),
      ?assert(lists:member("consistent", QArgs)),
      ?assertEqual("foo", proplists:get_value("dc", QArgs));
    {error, Reason} -> {error, Reason}
  end.

