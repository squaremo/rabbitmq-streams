-module(orchestrator_subprocess).

-export([start/3, handle_message/2, stop/1]).

-include("orchestrator.hrl").

-record(orchestrator_subprocess_state, {debug_info, port, output_acc, pid}).

start(DebugInfo, Port, ConfigDoc) ->
    error_logger:info_report({DebugInfo, start, [{pid, self()},
                                                 {port, Port},
                                                 {config_doc, ConfigDoc}]}),
    port_command(Port, rfc4627:encode(ConfigDoc) ++ "\n"),
    #orchestrator_subprocess_state{debug_info = DebugInfo,
                                   port = Port,
                                   output_acc = [],
                                   pid = undefined}.

handle_message({Port, {data, {eol, Fragment}}},
               State = #orchestrator_subprocess_state{debug_info = _DebugInfo,
                                                      port = Port,
                                                      pid = undefined}) ->
    Pid = list_to_integer(Fragment),
    ?DEBUGREPORT({_DebugInfo, pid_received, Pid}),
    {ok, State#orchestrator_subprocess_state{pid = Pid}};
handle_message({Port, {data, {noeol, Fragment}}},
               State = #orchestrator_subprocess_state{port = Port}) ->
    {ok, append_fragment(Fragment, State)};
handle_message({Port, {data, {eol, Fragment}}},
               State = #orchestrator_subprocess_state{port = Port}) ->
    {ok, append_fragment(Fragment ++ "\n", State)};
handle_message({'EXIT', Port, Reason},
               State = #orchestrator_subprocess_state{debug_info = _DebugInfo,
                                                      port = Port}) ->
    ?DEBUGREPORT({_DebugInfo, subprocess_terminated, Reason}),
    {error, Reason, State#orchestrator_subprocess_state{port = undefined}};
handle_message(Message, State) ->
    {error, {?MODULE, unhandled_message, Message}, State}.

append_fragment(Fragment, State = #orchestrator_subprocess_state{debug_info = _DebugInfo,
                                                                 output_acc = Acc}) ->
    ?DEBUGREPORT({_DebugInfo, received_fragment, Fragment}),
    State#orchestrator_subprocess_state{output_acc = [Fragment | Acc]}.

stop(#orchestrator_subprocess_state{debug_info = DebugInfo,
                                    port = Port,
                                    output_acc = Acc,
                                    pid = Pid}) ->
    error_logger:info_report({DebugInfo, stop, [{pid, Pid},
                                                {output, lists:flatten(lists:reverse(Acc))}]}),
    close_port(Port),
    kill_pid(Pid),
    ok.

close_port(undefined) -> true;
close_port(P) -> true = port_close(P).

kill_pid(undefined) -> ok;
kill_pid(P) -> os:cmd("kill " ++ integer_to_list(P)), ok.
