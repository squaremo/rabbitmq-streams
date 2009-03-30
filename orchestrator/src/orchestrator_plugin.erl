-module(orchestrator_plugin).

-behaviour(gen_server).

-export([start_link/1]).
-export([describe/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("orchestrator.hrl").

start_link(InitParameters) ->
    gen_server:start_link(?MODULE, InitParameters, []).

describe(PluginId) ->
    case file:read_file(plugin_path(PluginId, "plugin.js")) of
        {error, _} ->
            plugin_not_found(PluginId);
        {ok, JsonBin} ->
            {ok, Description, ""} = rfc4627:decode(JsonBin),
            {ok, Description}
    end.

%%---------------------------------------------------------------------------

plugin_path(PluginId, PathTail) ->
    %% TODO: Decide on path layout. Should plugins live in our priv_dir?
    orchestrator:priv_dir()++"/../../plugins/"++PluginId++"/"++PathTail.

harness_path(HarnessType, PathTail) ->
    %% TODO: Decide on path layout. Should harnesses live in our priv_dir?
    orchestrator:priv_dir()++"/../../harness/"++HarnessType++"/"++PathTail.

plugin_not_found(PluginId) ->
    error_logger:warning_report({?MODULE, plugin_not_found, PluginId}),
    not_found.

%%---------------------------------------------------------------------------

-record(state, {port, output_acc}).

%%---------------------------------------------------------------------------

init([HarnessTypeBin, PluginType, NodeId, Queues, Exchanges]) ->
    error_logger:info_report({?MODULE, starting_plugin, HarnessTypeBin, PluginType, NodeId, Queues, Exchanges}),
    HarnessType = binary_to_list(HarnessTypeBin),
    HarnessDir = harness_path(HarnessType, ""),
    error_logger:info_report({?MODULE, harness_dir, HarnessDir}),
    process_flag(trap_exit, true),
    Port = open_port({spawn, "./run_plugin.sh"},
                     [{line, 1048576},
                      use_stdio,
                      stderr_to_stdout,
                      {cd, HarnessDir}]),
    port_command(Port,
                 rfc4627:encode({obj,
                                 [{"harness_type", HarnessTypeBin},
                                  {"plugin_type", list_to_binary(PluginType)},
                                  {"plugin_dir", list_to_binary(plugin_path(PluginType, ""))},
                                  {"config",
                                   {obj,
                                    [{"node_id", list_to_binary(NodeId)},
                                     {"queues", [list_to_binary(Q) || Q <- Queues]},
                                     {"exchanges", [list_to_binary(X) || X <- Exchanges]}
                                    ]}}]}) ++ "\n"),
    {ok, #state{port = Port,
                output_acc = []}}.

handle_call(_Message, _From, State) ->
    {stop, unhandled_call, State}.

handle_cast(_Message, State) ->
    {stop, unhandled_cast, State}.

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
handle_info(_Info, State) ->
    {stop, unhandled_info, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
