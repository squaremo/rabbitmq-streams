-module(streams).

-export([all_pipelines/1, defn_doc/1, status_doc/1, process_status/1, activate/2, deactivate/2]).

-include("orchestrator.hrl").

defn_doc(ThingId) ->
    couchapi:get(streams_config:config_db() ++ "/" ++ ThingId).

status_doc_url(ThingId) when is_binary(ThingId) ->
    status_doc_url(binary_to_list(ThingId));
status_doc_url(ThingId) ->
    streams_config:config_db() ++ "/" ++ ThingId ++ "_status".

status_doc(ThingId) ->
    couchapi:get(status_doc_url(ThingId)).

set_status(ThingId, Active) ->
    case status_doc(ThingId) of
        {ok, Doc} ->
            case rfc4627:get_field(Doc, "active") of
                {ok, Active} ->
                    {ok, Active};
                Other ->
                    Doc2 = rfc4627:set_field(Doc, "active", Active),
                    {ok, _} = couchapi:put(status_doc_url(ThingId), Doc2),
                    {ok, Active}
            end;
        NotFound -> NotFound
    end.

activate(pipeline, ThingId) ->
    {ok, true} = set_status(ThingId, true),
    gen_server:cast(orchestrator_root, {status_change, ThingId}),
    ok.

deactivate(pipeline, ThingId) ->
    {ok, false} = set_status(ThingId, false),
    gen_server:cast(orchestrator_root, {status_change, ThingId}),
    ok.

all_pipelines(DbName) ->
    Rows = couchapi:get_view_rows(DbName, "feeds", "join?group=true"),
    Rows.

process_status(ThingId) ->
    lists:any(fun
                  ({Id, _, _, _}) when Id==ThingId -> true;
                  (_) -> false
              end,
              supervisor:which_children(orchestrator_root_sup)).
