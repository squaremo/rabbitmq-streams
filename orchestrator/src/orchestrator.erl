-module(orchestrator).

-export([start/0, stop/0, start/2, stop/1]).

start() -> application:start(?MODULE).
stop() -> application:stop(?MODULE).

start(normal, []) ->
    this_is_an_invalid_response. %% see http://www.erlang.org/doc/design_principles/applications.html#7.2

stop(_State) ->
    ok.
