-module(orchestrator).

-export([start/0, stop/0, start/2, stop/1]).

start() -> application:start(?MODULE).
stop() -> application:stop(?MODULE).

start(normal, []) ->
    orchestrator_root_sup:start_link().

stop(_State) ->
    ok.
