-module(conserl_acceptance_tests).

-include("conserl.hrl").

-include_lib("eunit/include/eunit.hrl").

simple_kv_put_get_test() ->
  application:ensure_all_started(conserl),
  Key = uuid:to_string(uuid:v4()),
  Value = uuid:to_string(uuid:v4()),
  conserl_kv:put(Key, Value),
  {ok, Response} = conserl_kv:get(Key),
  ?assertEqual(Value, maps:get(value, Response)).

simple_kv_get_not_found_test() ->
  Key = uuid:to_string(uuid:v4()),
  ?assertEqual({error, "Not Found"}, conserl_kv:get(Key)).



