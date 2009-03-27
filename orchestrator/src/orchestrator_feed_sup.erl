-module(orchestrator_feed_sup).

-behaviour(supervisor).

-export([start_link/1]).
-export([init/1]).

-define(SERVER, ?MODULE).

start_link(FeedId) ->
    supervisor:start_link(?MODULE, [FeedId]).

init([FeedId]) ->
    {ok, {{one_for_one, 10, 10},
	  [{orchestrator_feed, {orchestrator_feed, start_link, [FeedId]},
            permanent, 5000, worker, [orchestrator_feed]}]}}.
