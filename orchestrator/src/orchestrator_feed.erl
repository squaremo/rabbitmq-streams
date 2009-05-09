-module(orchestrator_feed).

-behaviour(gen_server).

-export([start_link/2]).
-export([selfcheck/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("orchestrator.hrl").
-include("rabbit_framing.hrl").

start_link(FeedId, FeedSupPid) ->
    gen_server:start_link(?MODULE, [FeedId, FeedSupPid], []).

selfcheck(FeedPid) ->
    ok = gen_server:cast(FeedPid, selfcheck).

%%---------------------------------------------------------------------------

-record(state, {feed_id, channel, config_rev_id, feed_sup_pid, plugin_sup_pid}).

-record(node_configuration, {node_id, plugin_config, plugin_desc, queues, exchanges, database}).

open_channel(State = #state{channel = undefined}) ->
    {ok, Ch} = orchestrator_root:open_channel(),
    State#state{channel = Ch}.

ensure_plugin_sup_detected(State = #state{plugin_sup_pid = P}) when is_pid(P) ->
    State;
ensure_plugin_sup_detected(State = #state{feed_sup_pid = FSP}) ->
    case lists:keysearch(orchestrator_plugin_sup, 1, supervisor:which_children(FSP)) of
        {value, {_Id, PluginSupPid, supervisor, _Modules}} ->
            State#state{plugin_sup_pid = PluginSupPid};
        false ->
            exit(plugin_sup_not_found)
    end.

get_config(#state{feed_id = FeedId}) ->
    {ok, FeedDefinition} = couchapi:get(?FEEDSHUB_STATUS_DBNAME ++ binary_to_list(FeedId)),
    FeedDefinition.

resource_name(FeedId, N, A) when is_binary(FeedId) ->
    resource_name(binary_to_list(FeedId), N, A);
resource_name(F, NodeId, A) when is_binary(NodeId) ->
    resource_name(F, binary_to_list(NodeId), A);
resource_name(F, N, AttName) when is_binary(AttName) ->
    resource_name(F, N, binary_to_list(AttName));
resource_name(FeedIdStr, NodeIdStr, AttachmentNameStr) ->
    FeedIdStr ++ "_" ++ NodeIdStr ++ "_" ++ AttachmentNameStr.

do_start_pipeline(State = #state{feed_id = FeedIdBin,
                                 channel = Channel,
                                 plugin_sup_pid = PluginSupPid}) ->
    FeedId = binary_to_list(FeedIdBin),
    FeedDefinition = get_config(State),
    {ok, Wiring} = rfc4627:get_field(FeedDefinition, "wiring"),
    {ok, {obj, NodeDefs}} = rfc4627:get_field(Wiring, "nodes"),
    {ok, Edges} = rfc4627:get_field(Wiring, "edges"),

    couchapi:createdb("feed_" ++ FeedId), %% database per feed

    error_logger:info_report({?MODULE, start_pipeline,
                              {nodes, NodeDefs},
                              {edges, Edges}}),

    %% Go through all nodes creating resources (queues, exchanges).
    NodeConfigurations = [node_configuration(Channel, FeedId, N) || N <- NodeDefs],

    %% Go through all edges creating bindings.
    lists:foreach(fun(Edge) -> bind_edges(FeedId, Channel, Edge, NodeDefs) end, Edges),

    %% Finally, go through nodes again starting the plugin instances.
    lists:foreach(fun (NodeConfiguration) ->
                          start_component(PluginSupPid, FeedId, NodeConfiguration)
                  end, lists:filter(fun (NC) -> NC /= ok end, NodeConfigurations)),

    State.

bind_edges(FeedId, Channel, EdgeJson, NodeDefs) ->
    {ok, FromJson} = rfc4627:get_field(EdgeJson, "from"),
    {ok, ToJson} = rfc4627:get_field(EdgeJson, "to"),
    {ok, FromNode} = rfc4627:get_field(FromJson, "node"),
    {Exchange, RK} =
	case rfc4627:get_field(FromJson, "channel") of
	    {ok, FromChannel} -> {list_to_binary(resource_name(FeedId, FromNode, FromChannel)), <<>>};
	    _ -> %% reading from a terminal. But we need to find the server
		{ok, TerminalNode} = rfc4627:get_field({obj, NodeDefs}, binary_to_list(FromNode)),
		{ok, TerminalName} = rfc4627:get_field(TerminalNode, "terminal"),
		ServerName = orchestrator_server:find_server_for_terminal(TerminalName),
		{ServerName, TerminalName}
	end,
    {ok, ToNode} = rfc4627:get_field(ToJson, "node"),
    Queue = case rfc4627:get_field(ToJson, "channel") of
		{ok, ToChannel} -> list_to_binary(resource_name(FeedId, ToNode, ToChannel));
		_ -> %% destination is a terminal, we have an exchange per terminal
		    {ok, TerminalNode2} = rfc4627:get_field({obj, NodeDefs}, binary_to_list(ToNode)),
		    {ok, TerminalName2} = rfc4627:get_field(TerminalNode2, "terminal"),
		    TerminalName2
	    end,
    error_logger:info_report({?MODULE, bind_edges, "binding exchange " ++ (binary_to_list(Exchange)) ++ " to queue " ++ (binary_to_list(Queue)) ++ " with binding key " ++ (binary_to_list(RK))}),
    lib_amqp:bind_queue(Channel, Exchange, Queue, RK).

node_configuration(Channel, FeedId, {NodeId, NodeSpecJson}) ->
    case rfc4627:get_field(NodeSpecJson, "type") of
	{ok, PluginTypeBin} -> plugin_component_node(PluginTypeBin, Channel, FeedId, {NodeId, NodeSpecJson});
	_Err -> case rfc4627:get_field(NodeSpecJson, "terminal") of
		    {ok, TerminalNameBin} -> terminal_node(TerminalNameBin, Channel, FeedId, {NodeId, NodeSpecJson})
		end
    end.

terminal_node(_TerminalNameBin, _Channel, _FeedId, {_NodeId, _NodeSpecJson}) ->
    %% Actually, there's nothing to do here as we assume the terminal
    %% is on and the terminal or server exchanges are created when
    %% they start up
    ok.

plugin_component_node(PluginTypeBin, Channel, FeedId, {NodeId, NodeSpecJson}) ->
    PluginType = binary_to_list(PluginTypeBin),
    {ok, PluginTypeDescription} = orchestrator_plugin:describe(PluginType),
    error_logger:info_report({?MODULE, node_configuration, FeedId, NodeId, PluginTypeDescription}),
    {ok, Inputs} = rfc4627:get_field(PluginTypeDescription, "inputs_specification"),
    {ok, Outputs} = rfc4627:get_field(PluginTypeDescription, "outputs_specification"),
    Queues = [{binary_to_list(InputName), resource_name(FeedId, NodeId, InputName)} ||
                 Input <- Inputs,
                 {ok, InputName} <- [rfc4627:get_field(Input, "name")]],
    lists:foreach(fun ({_InputName, Q}) ->
                          amqp_channel:call(Channel,
                                            #'queue.declare'{queue = list_to_binary(Q),
                                                             durable = true})
                  end,
                  Queues),
    Exchanges = [{binary_to_list(OutputName), resource_name(FeedId, NodeId, OutputName)} ||
                    Output <- Outputs,
                    {ok, OutputName} <- [rfc4627:get_field(Output, "name")]],
    lists:foreach(fun ({_OutputName, X}) ->
                          amqp_channel:call(Channel,
                                            #'exchange.declare'{exchange = list_to_binary(X),
                                                                type = <<"fanout">>,
                                                                durable = true})
                  end,
                  Exchanges),
    DbName = case rfc4627:get_field(PluginTypeDescription, "database_specification") of
                 not_found ->
                     undefined;
                 {ok, null} ->
                     undefined;
                 {ok, {obj, InitialDocs}} ->
                     D = resource_name(FeedId, NodeId, "db/"),
                     couchapi:createdb(D), %% TODO
                     error_logger:warning_report({?MODULE, ignoring_initial_docs, InitialDocs}),
                     couchapi:expand(D)
             end,
    #node_configuration{node_id = NodeId,
                        plugin_config = NodeSpecJson,
                        plugin_desc = PluginTypeDescription,
                        queues = Queues,
                        exchanges = Exchanges,
                        database = DbName}.

start_component(PluginSupPid, FeedId,
                #node_configuration{node_id = NodeId,
                                    plugin_config = PluginConfig,
                                    plugin_desc = PluginTypeDescription,
                                    queues = Queues,
                                    exchanges = Exchanges,
                                    database = DbName}) ->
    {ok, HarnessTypeBin} = rfc4627:get_field(PluginTypeDescription, "harness"),
    supervisor:start_child(PluginSupPid, [[HarnessTypeBin,
                                           PluginConfig,
                                           PluginTypeDescription,
                                           FeedId,
                                           NodeId,
                                           Queues,
                                           Exchanges,
                                           DbName]]),
    ok.

do_selfcheck(State = #state{config_rev_id = CurrentConfigRev}) ->
    {ok, DbConfigRev} = rfc4627:get_field(get_config(State), "_rev"),
    if
        DbConfigRev == CurrentConfigRev ->
            {noreply, State};
        true ->
            error_logger:info_report({?MODULE, selfcheck, restarting,
                                      [{old_rev, CurrentConfigRev},
                                       {new_rev, DbConfigRev}]}),
            {stop, normal, State}
    end.

%%---------------------------------------------------------------------------

init([FeedId, FeedSupPid]) ->
    %% We send ourselves a message, because we need to interact with
    %% FeedSupPid, our parent supervisor, and doing so during init is
    %% a violation of the behaviour spec required of us as a
    %% supervisee. It deadlocks the supervisor.
    gen_server:cast(self(), start_pipeline),
    {ok, #state{feed_id = FeedId,
                channel = undefined,
                config_rev_id = null,
                feed_sup_pid = FeedSupPid,
                plugin_sup_pid = undefined}}.

handle_call(_Message, _From, State) ->
    {stop, unhandled_call, State}.

handle_cast(start_pipeline, State) ->
    {noreply, do_start_pipeline(open_channel(ensure_plugin_sup_detected(State)))};
handle_cast(selfcheck, State) ->
    do_selfcheck(State);
handle_cast(_Message, State) ->
    {stop, unhandled_cast, State}.

handle_info(_Info, State) ->
    {stop, unhandled_info, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
