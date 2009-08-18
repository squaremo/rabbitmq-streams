-module(streams).

-export([all_pipelines/1, defn_doc/1, status_doc/1, process_status/1]).

-include("orchestrator.hrl").

defn_doc(ThingId) ->
    couchapi:get(streams_config:config_db() ++ "/" ++ ThingId).

status_doc(ThingId) ->
    couchapi:get(streams_config:config_db() ++ "/" ++ ThingId ++ "_status").

all_pipelines(DbName) ->
    Rows = couchapi:get_view_rows(DbName, "feeds", "join?group=true"),
    Rows.

process_status(ThingId) ->
    lists:any(fun
                  ({Id, _, _, _}) when Id==ThingId -> true;
                  (_) -> false
              end,
              supervisor:which_children(orchestrator_root_sup)).
