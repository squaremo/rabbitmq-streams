-module(orchestrator_server).

-behaviour(gen_server).

-export([start_link/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("orchestrator.hrl").
-include("rabbit_framing.hrl").

start_link(ServerId, ServerSupPid) ->
    gen_server:start_link(?MODULE, [ServerId, ServerSupPid], []).

-record(state, {port, output_acc, plugin_pid, server_id, server_sup_pid}).

init([ServerId, ServerSupPid]) ->
    #state {}.

handle_call(_Message, _From, State) ->
    {stop, unhandled_call, State}.

handle_cast(_Message, State) ->
    {stop, unhandled_cast, State}.

handle_info(_Info, State) ->
    {stop, unhandled_info, State}.

terminate(_Reason, #state{port = Port, output_acc = Acc, plugin_pid = PluginPid}) ->
    error_logger:info_report({?MODULE, server_terminating, lists:flatten(lists:reverse(Acc))}),
    true = port_close(Port),
    if undefined =:= PluginPid -> true;
       true -> os:cmd("kill "++(integer_to_list(PluginPid)))
    end,
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
