-module(orchestrator_server_sup).

-behaviour(supervisor).

-export([start_link/9]).
-export([init/1]).

start_link(ServerId,
	   PipelineChannel, PipelineBroker,
	   IngressChannel, IngressBroker,
	   EgressChannel, EgressBroker,
	   RootPid, AmqpConfig) when is_binary(ServerId) ->
    supervisor:start_link({local, list_to_atom("Server_" ++ binary_to_list(ServerId))},
			  ?MODULE, [ServerId,
				    PipelineChannel, PipelineBroker,
				    IngressChannel, IngressBroker,
				    EgressChannel, EgressBroker,
				    RootPid, AmqpConfig]).

init([ServerId,
      PipelineChannel, PipelineBroker,
      IngressChannel, IngressBroker,
      EgressChannel, EgressBroker,
      RootPid, AmqpConfig]) ->
    {ok, {{one_for_all, 0, 10},
	  [{orchestrator_server, {orchestrator_server, start_link, [self(), ServerId,
								    PipelineChannel, PipelineBroker,
								    IngressChannel, IngressBroker,
								    EgressChannel, EgressBroker,
								    RootPid, AmqpConfig]},
            permanent, 5000, worker, [orchestrator_server]}]}}.
