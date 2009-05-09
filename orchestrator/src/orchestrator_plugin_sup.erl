-module(orchestrator_plugin_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link(?MODULE, []).

init([]) ->
    {ok, {{one_for_one, 10, 10}, []}}.
