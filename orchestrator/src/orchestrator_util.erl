-module(orchestrator_util).

-export([amqp_plugin_config/1, uptime/0]).

-include("orchestrator.hrl").

amqp_plugin_config(#amqp_config { host=Host,
                                  port=Port,
                                  virtual_host=VHost,
                                  user=User,
                                  password=Password }) ->
    {"messageserver",
     {obj, [{"host", list_to_binary(Host)},
            {"port", Port},
            {"virtual_host", VHost},
            {"username", list_to_binary(User)},
            {"password", list_to_binary(Password)}
           ]}}.

uptime() ->
    {Millis, _} = statistics(wall_clock),
    {Millis, format_uptime(Millis)}.

%% An inexact but OK representation of uptime
format_uptime(Total) ->
    MillisPerHour = 1000 * 3600,
    MilliSecondsPerDay = MillisPerHour * 24,
    Days = Total div (MilliSecondsPerDay),
    RemMillis = Total rem MilliSecondsPerDay,
    Hours = RemMillis div MillisPerHour,
    RemMillis2 = RemMillis rem MillisPerHour,
    Minutes = RemMillis2 div 60000,
    lists:flatten(io_lib:format("~p days, ~p hours, ~2.10.0B minutes, ~2.10.0B seconds.",
                                [Days,
                                 Hours,
                                 Minutes,
                                 RemMillis2 rem 60000 div 1000])).
