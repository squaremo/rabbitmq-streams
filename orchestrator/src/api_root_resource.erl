-module(api_root_resource).

-export([init/1]).
-export([content_types_provided/2, app_status_json/2]).

-include("webmachine.hrl").
-include("api.hrl").

init([]) ->
    {ok, undefined}.

allowed_methods(Req, Context) ->
    {['GET', 'HEAD'], Req, Context}.

content_types_provided(Req, Context) ->
    {[{"application/json", app_status_json}, {"text/plain", app_status_json}],
     Req, Context}.

app_status_json(Req, Context) ->
    {rfc4627:encode(app_status()), Req, Context}.

app_status() ->
    {obj, [{"application", ?APPLICATION_NAME},
           {"version", ?APPLICATION_VERSION},
           {"uptime", format_uptime(statistics(wall_clock))}]}.

%% An inexact but OK representation of uptime
format_uptime({Total, _}) ->
    MillisPerHour = 1000 * 3600,
    MilliSecondsPerDay = MillisPerHour * 24,
    Days = Total div (MilliSecondsPerDay),
    RemMillis = Total rem MilliSecondsPerDay,
    Hours = RemMillis div MillisPerHour,
    RemMillis2 = RemMillis rem MillisPerHour,
    Minutes = RemMillis2 div 60000,
    {obj, [{milliseconds, Total},
           {as_string, list_to_binary(
                         io_lib:format("~p days, ~p hours, ~2.10.0B minutes, ~2.10.0B seconds.",
                                       [Days,
                                        Hours,
                                        Minutes,
                                        RemMillis2 rem 60000 div 1000]))}]}.
