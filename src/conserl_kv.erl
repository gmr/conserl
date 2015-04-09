%%% @author Gavin M. Roy <gavinmroy@gmail.com>
%%% @copyright 2015, Gavin M. Roy
%%% @doc Consul KV API endpoints

-module(conserl_kv).

-export([get/1, get/2]).

get(Key) ->
  get(Key, []).

get(Key, QArgs) ->
  Result = gen_server:call(conserl, {get, [kv, Key], QArgs}),
  case maps:get(status_code, Result) of
    200 -> {ok, build_get_response(Result)};
    404 -> {error, not_found}
  end.

build_get_response(Result) ->
  [Payload] = jsx:decode(maps:get(body, Result)),
  Value = proplists:get_value(<<"Value">>, Payload),
  #{create_index => proplists:get_value(<<"CreateIndex">>, Payload),
    modify_index => proplists:get_value(<<"ModifyIndex">>, Payload),
    lock_index => proplists:get_value(<<"LockIndex">>, Payload),
    key => binary_to_list(proplists:get_value(<<"Key">>, Payload)),
    flags => proplists:get_value(<<"Flags">>, Payload),
    value => base64:decode_to_string(binary_to_list(Value))}.
