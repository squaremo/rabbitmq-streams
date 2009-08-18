-module(orchestrator).

-export([start/0, stop/0, start/2, stop/1]).
-export([priv_dir/0]).

start() -> application:start(?MODULE).
stop() -> application:stop(?MODULE).

start(normal, []) ->
    {ok, _} = ibrowse_sup:start_link(),
    api_deps:ensure(),
    orchestrator_root_sup:start_link().

stop(_State) ->
    ok.

priv_dir() ->
    case code:priv_dir(?MODULE) of
        {error, bad_name} ->
            {ok, Cwd} = file:get_cwd(),
            Cwd ++ "/priv";
        D ->
            D
    end.
