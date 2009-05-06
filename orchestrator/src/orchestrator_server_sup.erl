-module(orchestrator_server_sup).

-behaviour(supervisor).

-export([start_link/1]).
-export([init/1]).

-define(SERVER, ?MODULE).

start_link(ServerId) when is_binary(ServerId) ->
    supervisor:start_link({local, list_to_atom("Server_" ++ binary_to_list(ServerId))},
			  ?MODULE, [ServerId]).

%% In some ways, this should probably be one_for_one or even simple_one_for_one.
%% However, it's left as one_for_all in case other children are added under this
%% supervisor (?), but mainly because switching to simple_one_for_one would cause
%% the _root to have to treat starting/stopping feeds and servers differently
init([ServerId]) ->
    {ok, {{one_for_all, 10, 10},
	  [{orchestrator_server, {orchestrator_server, start_link, [ServerId, self()]},
            permanent, 5000, worker, [orchestrator_server]}]}}.
