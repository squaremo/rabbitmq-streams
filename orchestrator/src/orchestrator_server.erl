-module(orchestrator_server).

-behaviour(gen_server).

-export([start_link/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("orchestrator.hrl").
-include("rabbit_framing.hrl").

start_link(ServerId, ServerSupPid) ->
    gen_server:start_link(?MODULE, [ServerId, ServerSupPid], []).

-record(state, {port, output_acc, server_pid, server_id, server_sup_pid}).

get_server_instance_config(ServerId) when is_binary(ServerId) ->
    {ok, ServerInstanceConfig} = couchapi:get(?FEEDSHUB_STATUS_DBNAME ++ binary_to_list(ServerId)),
    ServerInstanceConfig.

get_server_static_config(ServerId) when is_binary(ServerId) ->
    case file:read_file(orchestrator_plugin:plugin_path(ServerId, "plugin.js")) of
        {error, _} ->
            orchestrator_plugin:plugin_not_found(ServerId);
        {ok, JsonBin} ->
            {ok, Description, ""} = rfc4627:decode(JsonBin),
            {ok, Description}
    end.

init([ServerId, ServerSupPid]) when is_binary(ServerId) ->
    {ok, empty_state}.

handle_call(_Message, _From, State) ->
    {stop, unhandled_call, State}.

handle_cast(_Message, State) ->
    {stop, unhandled_cast, State}.

handle_info(_Info, State) ->
    {stop, unhandled_info, State}.

terminate(_Reason, #state{port = Port, output_acc = Acc, server_pid = ServerPid}) ->
    error_logger:info_report({?MODULE, server_terminating, lists:flatten(lists:reverse(Acc))}),
    true = port_close(Port),
    if undefined =:= ServerPid -> true;
       true -> os:cmd("kill "++(integer_to_list(ServerPid)))
    end,
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
