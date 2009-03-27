-module(feedshub_plugin).

-export([describe/1]).

plugin_not_found(PluginId) ->
    error_logger:warning_report({?MODULE, plugin_not_found, PluginId}),
    not_found.

describe(PluginId) ->
    %% TODO: Decide on path layout. Should plugins live in our priv_dir?
    case file:read_file(orchestrator:priv_dir()++"/../../plugins/"++PluginId++"/plugin.js") of
        {error, _} ->
            plugin_not_found(PluginId);
        {ok, JsonBin} ->
            {ok, Description, ""} = rfc4627:decode(JsonBin),
            {ok, Description}
    end.
