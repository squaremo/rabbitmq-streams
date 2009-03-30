-module(orchestrator_plugin).

-behaviour(gen_server).

-export([start_link/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("orchestrator.hrl").

start_link(InitParameters) ->
    gen_server:start_link(?MODULE, InitParameters, []).

%%---------------------------------------------------------------------------

-record(state, {}).

%%---------------------------------------------------------------------------

init([HarnessTypeBin, PluginType, Queues, Exchanges]) ->
    error_logger:info_report({?MODULE, starting_plugin, HarnessTypeBin, PluginType, Queues, Exchanges}),
    {ok, #state{}}.

handle_call(_Message, _From, State) ->
    {stop, unhandled_call, State}.

handle_cast(_Message, State) ->
    {stop, unhandled_cast, State}.

handle_info(_Info, State) ->
    {stop, unhandled_info, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
