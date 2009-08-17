-module(orchestrator_root_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    {ok, {{one_for_one, 0, 10},
	  [{orchestrator_root, {orchestrator_root, start_link, []},
            permanent, 5000, worker, [orchestrator_root]},
           {api_sup, {api_sup, start_link, []},
            permanent, infinity, supervisor, [api_sup]}]}}.
