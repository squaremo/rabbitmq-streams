-module(api_model_pipeline_resource).

-export([init/1]).
-export([content_types_provided/2, resource_exists/2, allowed_methods/2, to_json/2]).
-export([generate_etag/2]).

-include("webmachine.hrl").
-include("api.hrl").

-record(state, { doc = undefined }).

init([]) ->
    {ok, #state{}}.

allowed_methods(Req, State) ->
    {['GET', 'HEAD'], Req, State}.

resource_exists(Req, State) ->
    case streams:defn_doc(wrq:path_info(id, Req)) of
        {ok, Doc} -> {true, Req, State#state{ doc = Doc }};
        {error, _, _} -> {false, Req, State}
    end.

content_types_provided(Req, State) ->
    {[{"application/json", to_json},
      {"text/plain", to_json}],
     Req, State}.

to_json(Req, State = #state { doc = Doc }) ->
    {rfc4627:encode(del_field(del_field(Doc, "_rev"), "_id")), Req, State}.

generate_etag(Req, State = #state { doc = Doc }) ->
    {case rfc4627:get_field(Doc, "_rev", undefined) of
         undefined -> undefined;
         BinRev -> binary_to_list(BinRev)
     end, Req, State}.

del_field({obj, Props}, Key) ->
    {obj, lists:keydelete(Key, 1, Props)}.
%% NB erlang_rfc4627 in general lets other cases badmatch; see set_field/3 e.g..
