-module(api_model_index_resource).

-export([init/1]).
-export([content_types_provided/2, allowed_methods/2, index_json/2]).

-include("webmachine.hrl").
-include("api.hrl").

-record(state, {kind}).

init([{kind, Kind}]) ->
    {ok, #state{kind=Kind}}.

allowed_methods(Req, State = #state{kind = pipeline}) ->
    {['GET', 'HEAD'], Req, State}.

content_types_provided(Req, State) ->
    {[{"application/json", index_json},
      {"text/plain", index_json}],
     Req, State}.

index_json(Req, State = #state{kind = Kind}) ->
    {rfc4627:encode(index(Kind)), Req, State}.

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
