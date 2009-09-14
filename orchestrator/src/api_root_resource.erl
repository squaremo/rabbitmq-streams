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
    {Millis, Str} = orchestrator_util:uptime(),
    {obj, [{"application", ?APPLICATION_NAME},
           {"version", ?APPLICATION_VERSION},
           {"uptime", {obj, [{milliseconds, Millis},
                             {as_string, list_to_binary(Str)}]}}]}.
