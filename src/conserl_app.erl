%% @hidden
-module(conserl_app).
-behavior(application).

-export([
         start/2,
         stop/1
        ]).

-spec start(term(), term()) -> {error,_} | {ok,pid()} | {ok,pid(),_}.
start(_StartType, _StartArgs) ->
    conserl_sup:start_link().

-spec stop(term()) -> ok.
stop(_State) ->
    ok.
