-module(orchestrator_server_sup).

-behaviour(supervisor).

-export([start_link/1]).
-export([init/1]).

-define(SERVER, ?MODULE).

start_link([ServerId,
	    PipelineChannel, PipelineBroker,
	    IngressChannel, IngressBroker,
	    EgressChannel, EgressBroker]) when is_binary(ServerId) ->
    supervisor:start_link({local, list_to_atom("Server_" ++ binary_to_list(ServerId))},
			  ?MODULE, [ServerId,
				    PipelineChannel, PipelineBroker,
				    IngressChannel, IngressBroker,
				    EgressChannel, EgressBroker]).

init([ServerId,
      PipelineChannel, PipelineBroker,
      IngressChannel, IngressBroker,
      EgressChannel, EgressBroker]) ->
    {ok, {{one_for_all, 10, 10},
	  [{orchestrator_server, {orchestrator_server, start_link, [self(), ServerId,
								    PipelineChannel, PipelineBroker,
								    IngressChannel, IngressBroker,
								    EgressChannel, EgressBroker]},
            permanent, 5000, worker, [orchestrator_server]}]}}.
