-module(orchestrator_subprocess).

-export([start/3, handle_message/2, stop/1]).

-include("orchestrator.hrl").

-record(orchestrator_subprocess_state, {debug_info, port, output_acc, output_acc_size=0, pid}).

-define(MAX_ACC_SIZE, 1024).

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
handle_message({Port, {data, {eol, Fragment}}}, State) ->
    handle_message({Port, {data, {noeol, Fragment ++ "\n"}}}, State);
handle_message({Port, {exit_status, 0}},
               State = #orchestrator_subprocess_state{port = Port}) ->
    {ok, State};
handle_message({Port, {exit_status, E}},
               State = #orchestrator_subprocess_state{port = Port,
                                                      debug_info = DebugInfo,
                                                      pid = Pid}) ->
    error_logger:error_report({?MODULE, subprocess_terminated,
                               io_lib:format("~p~n%% PID[~3w]: NONZERO EXITCODE: ~3w", [DebugInfo, Pid, E])}),
    {ok, State};
handle_message({'EXIT', Port, Reason},
               State = #orchestrator_subprocess_state{debug_info = _DebugInfo,
                                                      port = Port}) ->
    ?DEBUGREPORT({_DebugInfo, subprocess_terminated, Reason}),
    {error, Reason, State#orchestrator_subprocess_state{port = undefined}};
handle_message(Message, State) ->
    {error, {?MODULE, unhandled_message, Message}, State}.

append_fragment(Fragment, State = #orchestrator_subprocess_state{debug_info = DebugInfo,
                                                                 pid = Pid,
                                                                 output_acc = Acc,
                                                                 output_acc_size = Size}) ->
    ?DEBUGREPORT({DebugInfo, received_fragment, Fragment}),
    NewSize = Size + length(Fragment),
    case NewSize >= ?MAX_ACC_SIZE of
        true ->
            error_logger:info_msg("~p~n%% Subprocess output:~n~s~n", [{DebugInfo, output, [{pid, Pid}]}, lists:reverse(Acc)]),
            State#orchestrator_subprocess_state{output_acc = [], output_acc_size = 0};
        false ->
            State#orchestrator_subprocess_state{output_acc = [Fragment | Acc], output_acc_size = NewSize}
    end.

stop(#orchestrator_subprocess_state{debug_info = DebugInfo,
                                    port = Port,
                                    output_acc = Acc,
                                    pid = Pid}) ->
    error_logger:info_msg("~p~n%% Subprocess output:~n~s~n", [{DebugInfo, stop, [{pid, Pid}]}, lists:reverse(Acc)]),
    close_port(Port),
    kill_pid(Pid),
    ok.

close_port(undefined) -> true;
close_port(P) -> true = port_close(P).

kill_pid(undefined) -> ok;
kill_pid(P) -> os:cmd("kill " ++ integer_to_list(P)), ok.
