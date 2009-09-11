-module(streams).

-export([all_pipelines/0, defn_doc/1, status_doc/1]).
-export([set_defn/2, process_status/1, activate/2, deactivate/2]).
-export([new_id/0, create_defn/3]).

-include("orchestrator.hrl").

new_id() ->
    couchapi:uuid().

defn_doc(ThingId) ->
    couchapi:get(defn_doc_url(ThingId)).

set_defn(ThingId, JsonObj) ->
    %% FIXME This overwrites the document there regardless of
    %% revision.  This introduces a race.
    case defn_doc(ThingId) of
        {ok, Doc} ->
            Doc2 = case rfc4627:get_field(Doc, "_rev") of
                       {ok, Rev} -> rfc4627:set_field(JsonObj, "_rev", Rev);
                       _ -> Doc
                   end,
            couchapi:put(defn_doc_url(ThingId), Doc2);
        Error ->
            Error
    end.

create_defn(pipeline, Defn, Id) ->
    Url = defn_doc_url(Id),
    StatusUrl = status_doc_url(Id),
    StatusDoc = {obj, [{"type", <<"feed-status">>},
                       {"active", false}]},
    couchapi:put(Url, Defn),
    couchapi:put(StatusUrl, StatusDoc).

defn_doc_url(ThingId) when is_binary(ThingId) ->
    defn_doc_url(binary_to_list(ThingId));
defn_doc_url(ThingId) ->
    streams_config:config_db() ++ "/" ++ ThingId.

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

all_pipelines() ->
    %% This view is used because it is the only one that includes the whole document,
    %% and only includes feeds.
    Rows = couchapi:get_view_rows(streams_config:config_db(), "feeds", "join?group=true"),
    Rows.

process_status(ThingId) ->
    lists:any(fun
                  ({Id, _, _, _}) when Id==ThingId -> true;
                  (_) -> false
              end,
              supervisor:which_children(orchestrator_root_sup)).
