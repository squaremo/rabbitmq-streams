-module(orchestrator_feed_sup).

-behaviour(supervisor).

-export([start_link/4]).
-export([init/1]).

-define(SERVER, ?MODULE).

start_link(FeedId, PipelineBroker, EgressBroker, AmqpConfig) when is_binary(FeedId) ->
    supervisor:start_link({local, list_to_atom("Feed_" ++ binary_to_list(FeedId))},
			  ?MODULE, [FeedId, PipelineBroker, EgressBroker, AmqpConfig]).

init([FeedId, PipelineBroker, EgressBroker, AmqpConfig]) ->
    {ok, {{one_for_all, 0, 10},
	  [{orchestrator_plugin_sup, {orchestrator_plugin_sup, start_link, []},
            permanent, 5000, supervisor, [orchestrator_plugin_sup]},
           {orchestrator_feed, {orchestrator_feed, start_link, [FeedId, self(), PipelineBroker, EgressBroker, AmqpConfig]},
            permanent, 5000, worker, [orchestrator_feed]}]}}.
