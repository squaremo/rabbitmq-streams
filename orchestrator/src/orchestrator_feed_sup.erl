-module(orchestrator_feed_sup).

-behaviour(supervisor).

-export([start_link/1]).
-export([init/1]).

-define(SERVER, ?MODULE).

start_link(FeedId) when is_binary(FeedId) ->
    supervisor:start_link({local, list_to_atom("Feed_" ++ binary_to_list(FeedId))},
			  ?MODULE, [FeedId]);
start_link(FeedId) when is_list(FeedId) ->
    start_link(list_to_binary(FeedId)).

init([FeedId]) ->
    {ok, {{one_for_all, 10, 10},
	  [{orchestrator_plugin_sup, {orchestrator_plugin_sup, start_link, []},
            permanent, 5000, supervisor, [orchestrator_plugin_sup]},
           {orchestrator_feed, {orchestrator_feed, start_link, [FeedId, self()]},
            permanent, 5000, worker, [orchestrator_feed]}]}}.
