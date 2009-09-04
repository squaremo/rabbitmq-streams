-module(orchestrator_plugin).

-behaviour(gen_server).

-export([start_link/1]).
-export([describe/1, plugin_path/2, harness_path/2, plugin_not_found/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("orchestrator.hrl").
-include("rabbit_framing.hrl").
-include("rabbit.hrl").

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

-record(state, {subproc}).

%%---------------------------------------------------------------------------

init(_Args = [HarnessTypeBin, PluginConfig, PluginTypeConfig, FeedId, NodeId, Queues, Exchanges, DbName]) ->
    error_logger:info_report({?MODULE, starting_plugin, _Args}),
    {ok, PluginNameBin} = rfc4627:get_field(PluginConfig, "type"),
    {ok, PluginUserConfig}  = case rfc4627:get_field(PluginConfig, "configuration") of
				  {ok, PUC} -> {ok, PUC};
				  not_found -> {ok, {obj, []}}
			      end,
    PluginName = binary_to_list(PluginNameBin),
    HarnessType = binary_to_list(HarnessTypeBin),
    HarnessDir = harness_path(HarnessType, ""),
    process_flag(trap_exit, true),
    Port = open_port({spawn, "./run_plugin.sh -rmqs-name=" ++ PluginName},
                     [{line, 1048576},
                      use_stdio,
                      stderr_to_stdout,
                      {cd, HarnessDir}]),
    StateDocUrl = couchapi:expand("feed_" ++ FeedId ++ "/state_" ++ NodeId),
    PairsToBin = fun ({Key,Value}) -> {Key, list_to_binary(Value)} end,
    QueuesBin = lists:map(PairsToBin, Queues),
    ExchangesBin = lists:map(PairsToBin, Exchanges),
    {ok, Debug} = application:get_env(orchestrator, debug),
    {ok, Trace} = application:get_env(orchestrator, trace),
    ConfigDoc = {obj, 
                 [{"debug", Debug},
                  {"trace", Trace},
                  {"harness_type", HarnessTypeBin},
                  {"plugin_name", PluginNameBin},
                  {"plugin_dir", list_to_binary(plugin_path(PluginName, ""))},
                  {"feed_id", list_to_binary(FeedId)},
                  {"node_id", list_to_binary(NodeId)},
                  {"plugin_type", PluginTypeConfig},
		  {"global_configuration", {obj, []}}, %% TODO
                  {"configuration", PluginUserConfig},
                  {"messageserver",
                   {obj, [{"host", <<"localhost">>}, %% TODO thread thru from root config
                          {"port", 5672}, %% TODO thread thru from root config
                          {"virtual_host", <<"/">>}, %% TODO thread thru from root config
                          {"username", <<"feedshub_admin">>}, %% TODO use per-feed username
                          {"password", <<"feedshub_admin">>} %% TODO use per-feed username
                          ]}},
                  {"inputs", {obj, QueuesBin}},
                  {"outputs", {obj, ExchangesBin}},
                  {"state", list_to_binary(StateDocUrl)},
                  {"database", case DbName of
				   undefined -> null;
				   Url -> list_to_binary(Url)
			       end}
		 ]},
    {ok, #state{subproc = orchestrator_subprocess:start({?MODULE, PluginName, FeedId, NodeId},
                                                        Port,
                                                        ConfigDoc)}}.

handle_call(_Message, _From, State) ->
    {stop, unhandled_call, State}.

handle_cast(_Message, State) ->
    {stop, unhandled_cast, State}.

handle_info(Message, State = #state{subproc = S}) ->
    case orchestrator_subprocess:handle_message(Message, S) of
        {ok, S1} -> {noreply, State#state{subproc = S1}};
        {error, Reason, S1} -> {stop, Reason, State#state{subproc = S1}}
    end.

terminate(_Reason, #state{subproc = S}) ->
    ok = orchestrator_subprocess:stop(S).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
