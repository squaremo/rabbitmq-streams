-module(api_model_index_resource).

-export([init/1]).
-export([content_types_provided/2, allowed_methods/2, index_json/2]).
-export([post_is_create/2, create_path/2]).
-export([content_types_accepted/2, from_json/2]).

-include("webmachine.hrl").
-include("api.hrl").

init([{kind, Kind}]) ->
    {ok, #modelctx{ kind = Kind }}.

allowed_methods(Req, State = #modelctx{kind = pipeline}) ->
    {['GET', 'HEAD', 'POST'], Req, State}.

content_types_provided(Req, State) ->
    {[{"application/json", index_json},
      {"text/plain", index_json}],
     Req, State}.

content_types_accepted(Req, State) ->
    {[{"application/json", from_json}], Req, State}.

index_json(Req, State = #modelctx{kind = Kind}) ->
    {rfc4627:encode(index(Kind)), Req, State}.

post_is_create(Req, State = #modelctx { kind = pipeline }) ->
    {true, Req, State};
post_is_create(Req, State) ->
    {false, Req, State}.

create_path(Req, State = #modelctx{ kind = pipeline }) ->
    {streams:new_id(), Req, State}.

% ----------

%% Things dealing with POSTing we delegate

from_json(Req, State = #modelctx{ kind = pipeline }) ->
    Id = wrq:disp_path(Req),
    api_model_pipeline_resource:make_pipeline(Id, Req, State).

% ----------

index(pipeline) ->
    list_pipelines(?DEFAULT_PIPELINE_FIELDS).

list_pipelines(Fields) ->
    All = streams:all_pipelines(),
    {obj, [{total, length(All)},
           {values, [pipeline_row(P, Fields) || P <- All]}]}.

pipeline_row(Row, Fields) ->
    {ok, Id} = rfc4627:get_field(Row, "key"),
    {ok, PipelineJoin} = rfc4627:get_field(Row, "value"),
    {ok, PipelineDesc} = rfc4627:get_field(PipelineJoin, "feed"),
    FieldValues = lists:map(
                    fun (F) -> V = rfc4627:get_field(PipelineDesc, F, null),
                               {F, V} end,
                    Fields),
    PipelineUrl = api_util:url(model, pipeline, binary_to_list(Id)),
    {obj, [{url, list_to_binary(PipelineUrl)}, {value, {obj, FieldValues}}]}.
