-module(api_process_index_resource).

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

% -------

index(pipeline) ->
    list_pipelines_status().

list_pipelines_status() ->
    All = streams:all_pipelines(),
    {obj, [{total, length(All)},
           {values, [pipeline_status_row(P) || P <- All]}]}.

pipeline_status_row(Row) ->
    {ok, Id} = rfc4627:get_field(Row, "key"),
    {obj, [{url, list_to_binary(api_util:url(process, pipeline, binary_to_list(Id)))},
           {status, streams:process_status(Id)}]}.

