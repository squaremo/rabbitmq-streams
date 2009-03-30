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

-record(node_configuration, {node_id, plugin_type, plugin_desc, queues, exchanges}).

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
    {ok, FeedDefinition} = couchapi:get(?FEEDSHUB_CONFIG_DBNAME ++ binary_to_list(FeedId)),
    FeedDefinition.

resource_name(FeedId, N, A) when is_binary(FeedId) ->
    resource_name(binary_to_list(FeedId), N, A);
resource_name(F, NodeId, A) when is_binary(NodeId) ->
    resource_name(F, binary_to_list(NodeId), A);
resource_name(F, N, AttName) when is_binary(AttName) ->
    resource_name(F, N, binary_to_list(AttName));
resource_name(FeedIdStr, NodeIdStr, AttachmentNameStr) ->
    FeedIdStr ++ ":" ++ NodeIdStr ++ ":" ++ AttachmentNameStr.

do_start_pipeline(State = #state{feed_id = FeedIdBin,
                                 channel = Channel,
                                 plugin_sup_pid = PluginSupPid}) ->
    FeedId = binary_to_list(FeedIdBin),
    FeedDefinition = get_config(State),
    {ok, Wiring} = rfc4627:get_field(FeedDefinition, "wiring"),
    {ok, {obj, NodeDefs}} = rfc4627:get_field(Wiring, "nodes"),
    {ok, Edges} = rfc4627:get_field(Wiring, "edges"),

    error_logger:info_report({?MODULE, start_pipeline,
                              {nodes, NodeDefs},
                              {edges, Edges}}),

    %% Go through all nodes creating resources (queues, exchanges).
    NodeConfigurations = [node_configuration(Channel, FeedId, N) || N <- NodeDefs],

    %% Go through all edges creating bindings.
    lists:foreach(fun ([ONode, OAtt, INode, IAtt]) ->
                          lib_amqp:bind_queue(Channel,
                                              list_to_binary(resource_name(FeedId, ONode, OAtt)),
                                              list_to_binary(resource_name(FeedId, INode, IAtt)),
                                              <<>>)
                  end, Edges),

    %% Finally, go through nodes again starting the plugin instances.
    lists:foreach(fun (NodeConfiguration) ->
                          start_component(PluginSupPid, NodeConfiguration)
                  end, NodeConfigurations),

    State.

node_configuration(Channel, FeedId, {NodeId, NodeSpecJson}) ->
    {ok, PluginTypeBin} = rfc4627:get_field(NodeSpecJson, "type"),
    PluginType = binary_to_list(PluginTypeBin),
    {ok, PluginTypeDescription} = orchestrator_plugin:describe(PluginType),
    error_logger:info_report({?MODULE, node_configuration, FeedId, NodeId, PluginTypeDescription}),
    {ok, Inputs} = rfc4627:get_field(PluginTypeDescription, "inputs"),
    {ok, Outputs} = rfc4627:get_field(PluginTypeDescription, "outputs"),
    Queues = [resource_name(FeedId, NodeId, AttachmentName) ||
                 Input <- Inputs,
                 {ok, AttachmentName} <- [rfc4627:get_field(Input, "name")]],
    lists:foreach(fun (Q) ->
                          amqp_channel:call(Channel,
                                            #'queue.declare'{queue = list_to_binary(Q),
                                                             durable = true})
                  end,
                  Queues),
    Exchanges = [resource_name(FeedId, NodeId, AttachmentName) ||
                    Output <- Outputs,
                    {ok, AttachmentName} <- [rfc4627:get_field(Output, "name")]],
    lists:foreach(fun (X) ->
                          amqp_channel:call(Channel,
                                            #'exchange.declare'{exchange = list_to_binary(X),
                                                                type = <<"fanout">>,
                                                                durable = true})
                  end,
                  Exchanges),
    #node_configuration{node_id = NodeId,
                        plugin_type = PluginType,
                        plugin_desc = PluginTypeDescription,
                        queues = Queues,
                        exchanges = Exchanges}.

start_component(PluginSupPid, #node_configuration{node_id = NodeId,
                                                  plugin_type = PluginType,
                                                  plugin_desc = PluginTypeDescription,
                                                  queues = Queues,
                                                  exchanges = Exchanges}) ->
    {ok, HarnessTypeBin} = rfc4627:get_field(PluginTypeDescription, "harness"),
    supervisor:start_child(PluginSupPid, [[HarnessTypeBin,
                                           PluginType,
                                           NodeId,
                                           Queues,
                                           Exchanges]]),
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
