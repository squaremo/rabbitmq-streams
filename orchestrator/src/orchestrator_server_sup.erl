-module(orchestrator_server_sup).

-behaviour(supervisor).

-export([start_link/1]).
-export([init/1]).

-define(SERVER, ?MODULE).

start_link(ServerId) ->
    supervisor:start_link(?MODULE, [ServerId]).

init([ServerId]) ->
    {ok, {{one_for_all, 10, 10},
	  [{orchestrator_server, {orchestrator_server, start_link, [ServerId, self()]},
            permanent, 5000, worker, [orchestrator_server]}]}}.
