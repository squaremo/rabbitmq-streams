-module(orchestrator_plugin).

-behaviour(gen_server).

-export([start_link/1]).
-export([describe/1]).

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

-record(state, {port, output_acc, control_exchange_name, channel, plugin_pid}).

%%---------------------------------------------------------------------------

init(_Args = [HarnessTypeBin, PluginConfig, PluginTypeConfig, FeedId, NodeId, Queues, Exchanges, DbName, Channel]) ->
    error_logger:info_report({?MODULE, starting_plugin, _Args}),
    {ok, PluginTypeBin} = rfc4627:get_field(PluginConfig, "type"),
    {ok, PluginUserConfig}  = case rfc4627:get_field(PluginConfig, "configuration") of
				  {ok, PUC} -> {ok, PUC};
				  not_found -> {ok, {obj, []}}
			      end,
    PluginType = binary_to_list(PluginTypeBin),
    HarnessType = binary_to_list(HarnessTypeBin),
    HarnessDir = harness_path(HarnessType, ""),
    process_flag(trap_exit, true),
    Port = open_port({spawn, "./run_plugin.sh"},
                     [{line, 1048576},
                      use_stdio,
                      stderr_to_stdout,
                      {cd, HarnessDir}]),
    StateDocUrl = couchapi:expand("feed_" ++ FeedId ++ "/state_" ++ NodeId),
    ConfigDoc = {obj,
                 [{"harness_type", HarnessTypeBin},
                  {"plugin_name", PluginTypeBin},
                  {"plugin_dir", list_to_binary(plugin_path(PluginType, ""))},
                  {"feed_id", list_to_binary(FeedId)},
                  {"node_id", list_to_binary(NodeId)},
                  {"plugin_type", PluginTypeConfig},
                  {"configuration", PluginUserConfig},
                  {"messageserver",
                   {obj, [{"host", <<"localhost">>}, %% TODO thread thru from root config
                          {"port", 5672}, %% TODO thread thru from root config
                          {"virtual_host", <<"/">>}, %% TODO thread thru from root config
                          {"username", <<"feedshub_admin">>}, %% TODO use per-feed username
                          {"password", <<"feedshub_admin">>} %% TODO use per-feed username
                          ]}},
                  {"inputs", [list_to_binary(Q) || Q <- Queues]}, %% TODO thread thru queue names?
                  {"outputs", [list_to_binary(X) || X <- Exchanges]},
                  {"state", list_to_binary(StateDocUrl)},
                  {"database", case DbName of
                   undefined ->
                   null;
                   Url ->
                   list_to_binary(Url)
                   end}
                  ]},
    error_logger:info_report({?MODULE, config_doc, ConfigDoc}),
    port_command(Port, rfc4627:encode(ConfigDoc) ++ "\n"),
    {ok, #state{port = Port,
                output_acc = [],
		channel = Channel,
		plugin_pid = undefined
	       }}.

handle_call(_Message, _From, State) ->
    {stop, unhandled_call, State}.

handle_cast(_Message, State) ->
    {stop, unhandled_cast, State}.

handle_info({P, {data, {eol, Fragment}}}, State = #state{port = Port, plugin_pid = undefined})
  when P =:= Port ->
    {noreply, State #state {plugin_pid = list_to_integer(Fragment)}};
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

terminate(_Reason, #state{port = Port, output_acc = Acc, plugin_pid = PluginPid}) ->
    error_logger:info_report({?MODULE, plugin_terminating, lists:flatten(lists:reverse(Acc))}),
    true = port_close(Port),
    if undefined =:= PluginPid -> true;
       true -> os:cmd("kill "++(integer_to_list(PluginPid)))
    end,
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
