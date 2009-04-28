-module(orchestrator_logger).

-export([start_link/1]).

-define(SERVER, ?MODULE).

start_link(LogCh) ->
    case gen_event:start_link({local, orchestrator_logger}) of
	{ok, Pid} ->
	    ok = gen_event:add_handler(orchestrator_logger, orchestrator_terminal_logger, ["#", LogCh]),
	    {ok, Pid};
	{error,{already_started,Pid}} ->
	    {ok, Pid}
    end.
