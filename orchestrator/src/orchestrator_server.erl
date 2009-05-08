-module(orchestrator_server).

-behaviour(gen_server).

-export([start_link/9]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([find_server_for_terminal/1]).

-include("orchestrator.hrl").
-include("rabbit.hrl").
-include("rabbit_framing.hrl").

start_link(ServerSupPid, ServerId,
	   PipelineChannel, PipelineBroker,
	   IngressChannel, IngressBroker,
	   EgressChannel, EgressBroker,
	   RootPid) ->
    gen_server:start_link(?MODULE, [ServerSupPid, ServerId,
				    PipelineChannel, PipelineBroker,
				    IngressChannel, IngressBroker,
				    EgressChannel, EgressBroker,
				    RootPid], []).

find_server_for_terminal(TermId) when is_binary(TermId) ->
    case couchapi:get(?FEEDSHUB_STATUS_DBNAME ++ binary_to_list(TermId)) of
	{ok, Doc} ->
	    case rfc4627:get_field(Doc, "server") of
		{ok, Server} -> Server;
		Err ->
		    error_logger:error_report({?MODULE, find_server_for_terminal, Err, TermId}),
		    not_found
	    end;
	Err2 ->
	    error_logger:error_report({?MODULE, find_server_for_terminal, Err2, TermId}),
	    not_found
    end.

-record(state, {port, output_acc, server_pid, server_id, server_sup_pid, shovel_in_pid, shovel_out_pid, pipeline_channel}).

get_server_instance_config(ServerId) when is_list(ServerId) ->
    {ok, ServerInstanceConfig} = couchapi:get(?FEEDSHUB_STATUS_DBNAME ++ ServerId),
    ServerInstanceConfig.

get_server_static_config(ServerType) when is_list(ServerType) ->
    case file:read_file(orchestrator_plugin:plugin_path(ServerType, "plugin.js")) of
        {error, _} ->
            orchestrator_plugin:plugin_not_found(ServerType);
        {ok, JsonBin} ->
            {ok, Description, ""} = rfc4627:decode(JsonBin),
            Description
    end.

init([ServerSupPid, ServerIdBin,
      PipelineChannel, PipelineBroker,
      IngressChannel, IngressBroker,
      EgressChannel, EgressBroker,
      RootPid])
  when is_binary(ServerIdBin) ->
    gen_server:cast(self(), {start_server, ServerIdBin,
			     PipelineChannel, PipelineBroker,
			     IngressChannel, IngressBroker,
			     EgressChannel, EgressBroker,
			     RootPid}),

    ServerId = binary_to_list(ServerIdBin),
    {ok, #state{port = undefined,
                output_acc = [],
		server_pid = undefined,
		server_id = ServerId,
		server_sup_pid = ServerSupPid,
		shovel_in_pid = undefined,
		shovel_out_pid = undefined,
		pipeline_channel = undefined
	       }}.

handle_call(_Message, _From, State) ->
    {stop, unhandled_call, State}.

handle_cast({start_server, ServerIdBin, PipelineChannel, PipelineBroker,
	     IngressChannel, IngressBroker, EgressChannel, EgressBroker,
	     RootPid},
	    #state { server_id = ServerId, server_sup_pid = ServerSupPid }) ->

    ServerConfig = get_server_instance_config(ServerId),
    {ok, ServerTypeBin} = rfc4627:get_field(ServerConfig, "server_type"),
    ServerType = binary_to_list(ServerTypeBin),
    ServerDefinition = get_server_static_config(ServerType),

    CommandQueueName = ServerId ++ "_command",
    CommandQueueNameBin = list_to_binary(CommandQueueName),
    amqp_channel:call(PipelineChannel, #'queue.declare'{queue = CommandQueueNameBin,
							durable = false}),
    CQRK = list_to_binary(ServerId ++ ".*"),
    lib_amqp:bind_queue(PipelineChannel, ?FEEDSHUB_CONFIG_XNAME,
			CommandQueueNameBin, CQRK),

    CommandElem = {"command", CommandQueueNameBin},

    %% think very carefully about subjects and objects when understanding outputs and inputs!
    {ShovelOutPid, Inputs} =
	case rfc4627:get_field(ServerDefinition, "destination_specification") of
	    {ok, _} -> %% we are a destination
		OutNameBin = list_to_binary(ServerId ++ "_output"),
		amqp_channel:call(EgressChannel,
				  #'exchange.declare'{exchange = OutNameBin,
						      type = <<"direct">>,
						      durable = true}),
		amqp_channel:call(EgressChannel,
				  #'queue.declare'{queue = OutNameBin,
						   durable = true}),
		lib_amqp:bind_queue(EgressChannel, OutNameBin, OutNameBin, <<"#">>),

		Pid =
		    case supervisor:start_child(ServerSupPid,
						{egress_shovel,
						 {shovel, start_link, [PipelineBroker, undefined,
								       EgressBroker, OutNameBin]},
						 permanent,
						 brutal_kill,
						 worker,
						 [shovel]
						}) of
			{ok, Pid3} -> Pid3;
			{error, {already_started, Pid3}} -> Pid3;
			Err -> error_logger:error_report({?MODULE, start_shovel, ServerId, Err}),
			       error
		    end,

		%% we are a destination. This means we need to update
		%% the shovel whenever we see new terminals get turned
		%% on or off
		
		PrivateQ = lib_amqp:declare_private_queue(PipelineChannel),
		#'queue.bind_ok'{} = lib_amqp:bind_queue(PipelineChannel, ?FEEDSHUB_CONFIG_XNAME, PrivateQ, CQRK),

		{Pid, {obj, [{"input", OutNameBin}|CommandElem]}};
	    _ -> {undefined, {obj, [CommandElem]}}
	end,

    {ShovelInPid, Outputs} =
	case rfc4627:get_field(ServerDefinition, "source_specification") of
	    {ok, _} -> %% we are a source
		InNameBin = list_to_binary(ServerId ++ "_input"),
		amqp_channel:call(IngressChannel,
				  #'exchange.declare'{exchange = InNameBin,
						      type = <<"topic">>,
						      durable = true}),
		amqp_channel:call(IngressChannel,
				  #'queue.declare'{queue = InNameBin,
						   durable = true}),

		%% this is the exchange in the pipeline
		amqp_channel:call(PipelineChannel,
				  #'exchange.declare'{exchange = ServerIdBin,
						      type = <<"direct">>,
						      durable = true}),

		Pid2 =
		    case supervisor:start_child(ServerSupPid,
						{ingress_shovel,
						 {shovel, start_link, [IngressBroker, InNameBin,
								       PipelineBroker, ServerIdBin]},
						 permanent,
						 brutal_kill,
						 worker,
						 [shovel]
						}) of
			{ok, Pid4} -> Pid4;
			{error, {already_started, Pid4}} -> Pid4;
			Err2 -> error_logger:error_report({?MODULE, start_shovel, ServerId, Err2}),
			       error
		    end,

		shovel:bind_source_to_exchange(Pid2, {InNameBin, <<"#">>}, keep),
			
		{Pid2, {obj, [{"output", InNameBin}]}};
	_ -> {undefined, {obj, []}}
    end,	    

    {ok, HarnessTypeBin} = rfc4627:get_field(ServerDefinition, "harness"),
    HarnessType = binary_to_list(HarnessTypeBin),
    HarnessDir = orchestrator_plugin:harness_path(HarnessType, ""),
    process_flag(trap_exit, true),
    Port = open_port({spawn, "./run_plugin.sh"},
                     [{line, 1048576},
                      use_stdio,
                      stderr_to_stdout,
                      {cd, HarnessDir}]),

    {ok, ServerUserConfig}  = case rfc4627:get_field(ServerConfig, "configuration") of
				  {ok, PUC} -> {ok, PUC};
				  not_found -> {ok, {obj, []}}
			      end,

    ConfigDoc = {obj,
                 [{"harness_type", HarnessTypeBin},
                  {"plugin_name", ServerTypeBin},
                  {"plugin_dir", list_to_binary(orchestrator_plugin:plugin_path(ServerType, ""))},
                  {"server_id", ServerIdBin},
                  {"plugin_type", ServerDefinition},
		  {"global_configuration", {obj, []}}, %% TODO
                  {"configuration", ServerUserConfig},
                  {"messageserver", %% TODO - need more than one of these for the different brokers (potentially)
                   {obj, [{"host", <<"localhost">>}, %% TODO thread thru from root config
                          {"port", 5672}, %% TODO thread thru from root config
                          {"virtual_host", <<"/">>}, %% TODO thread thru from root config
                          {"username", <<"feedshub_admin">>}, %% TODO use per-feed username
                          {"password", <<"feedshub_admin">>} %% TODO use per-feed username
                          ]}},
                  {"inputs", Inputs},
                  {"outputs", Outputs},
                  {"database", list_to_binary(couchapi:expand("server_" ++ ServerId ++ "_state"))},
                  {"terminals_database", list_to_binary(couchapi:expand(?FEEDSHUB_STATUS_DBNAME))}
		 ]},
    error_logger:info_report({?MODULE, config_doc, ConfigDoc}),
    port_command(Port, rfc4627:encode(ConfigDoc) ++ "\n"),

    orchestrator_root:server_started_callback(RootPid),

    {noreply, #state{port = Port,
		     output_acc = [],
		     server_pid = undefined,
		     server_id = ServerId,
		     server_sup_pid = ServerSupPid,
		     shovel_in_pid = ShovelInPid,
		     shovel_out_pid = ShovelOutPid,
		     pipeline_channel = PipelineChannel
		    }};

handle_cast(_Message, State) ->
    {stop, unhandled_cast, State}.

handle_info({P, {data, {eol, Fragment}}}, State = #state{port = Port, server_pid = undefined})
  when P =:= Port ->
    {noreply, State #state {server_pid = list_to_integer(Fragment)}};
handle_info({P, {data, X}}, State = #state{port = Port, output_acc = Acc})
  when P =:= Port ->
    case X of
        {noeol, Fragment} ->
            {noreply, State#state{output_acc = [Fragment | Acc]}};
        {eol, Fragment} ->
            {noreply, State#state{output_acc = [Fragment ++ "\n" | Acc]}}
    end;
handle_info({'EXIT', P, Reason}, State = #state{port = Port, output_acc = Acc})
  when P =:= Port ->
    error_logger:error_report({?MODULE, plugin_exited, lists:flatten(lists:reverse(Acc))}),
    {stop, Reason, State};

handle_info(#'basic.consume_ok'{}, State) ->
    {noreply, State};
handle_info({#'basic.deliver' { exchange = ?FEEDSHUB_CONFIG_XNAME,
				'delivery_tag' = DeliveryTag,
				'routing_key' = RoutingKeyBin
			      },
	     #content { payload_fragments_rev = [<<"status change">>]}},
	    State = #state{ shovel_out_pid = ShovelOutPid, pipeline_channel = Ch, server_id = ServerId })
  when is_pid(ShovelOutPid) ->
    RoutingKey = binary_to_list(RoutingKeyBin),
    ServerId = lists:takewhile(fun (C) -> C /= $. end, RoutingKey),
    [$.|TerminalId] = lists:dropwhile(fun (C) -> C /= $. end, RoutingKey),
    TerminalIdBin = list_to_binary(TerminalId),
    amqp_channel:call(Ch,
		      #'exchange.declare'{exchange = TerminalIdBin,
					  type = <<"direct">>,
					  durable = true}),
    shovel:bind_source_to_exchange(ShovelOutPid, {TerminalIdBin, <<>>}, <<TerminalIdBin>>),
    lib_amqp:ack(Ch, DeliveryTag),
    {noreply, State};

handle_info(_Info, State) ->
    {stop, unhandled_info, State}.

terminate(_Reason, #state{port = Port, output_acc = Acc, server_pid = ServerPid}) ->
    error_logger:info_report({?MODULE, server_terminating, lists:flatten(lists:reverse(Acc))}),
    true =
	if Port == undefined -> true;
	   true -> port_close(Port)
	end,
    if undefined =:= ServerPid -> true;
       true -> os:cmd("kill "++(integer_to_list(ServerPid)))
    end,
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
