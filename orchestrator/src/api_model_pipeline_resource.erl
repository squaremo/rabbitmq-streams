-module(api_model_pipeline_resource).

-export([init/1]).
-export([content_types_provided/2, resource_exists/2, allowed_methods/2]).
-export([generate_etag/2, content_types_accepted/2, is_conflict/2]).
-export([to_json/2, from_json/2]).

-include("webmachine.hrl").
-include("api.hrl").

-record(state, { doc = undefined }).

init([]) ->
    {ok, #state{}}.

allowed_methods(Req, State) ->
    {['GET', 'HEAD', 'PUT'], Req, State}.

resource_exists(Req, State) ->
    case streams:defn_doc(wrq:path_info(id, Req)) of
        {ok, Doc} -> {true, Req, State#state{ doc = Doc }};
        {error, _, _} -> {false, Req, State}
    end.

content_types_provided(Req, State) ->
    {[{"application/json", to_json},
      {"text/plain", to_json}],
     Req, State}.

content_types_accepted(Req, State) ->
    {[{"application/json", from_json}], Req, State}.

to_json(Req, State = #state { doc = Doc }) ->
    {rfc4627:encode(del_field(del_field(Doc, "_rev"), "_id")), Req, State}.

from_json(Req, State) ->
    case rfc4627:decode(wrq:req_body(Req)) of
        {ok, Json, _} ->
            {put_pipeline(wrq:path_info(id, Req), Json), Req, State};
        {error, Reason} ->
            {{error, Reason}, Req, State}
    end.

%% I don't think this will arise, since it will fail at If-Match with 412 precondition failed
is_conflict(Req, State = #state{ doc = Doc }) ->
    {ok, Rev} = rfc4627:get_field(Doc, "_rev"),
    IfMatch = wrq:get_req_header("If-Match", Req),
    {binary_to_list(Rev) =/= IfMatch, Req, State}.  %% Ignores possibility of multiple values in the header

generate_etag(Req, State = #state { doc = Doc }) ->
    {case rfc4627:get_field(Doc, "_rev", undefined) of
         undefined -> undefined;
         BinRev -> binary_to_list(BinRev)
     end, Req, State}.

%% ---------

del_field({obj, Props}, Key) ->
    {obj, lists:keydelete(Key, 1, Props)}.
%% NB erlang_rfc4627 in general lets other cases badmatch; see set_field/3 e.g..

put_pipeline(Id, Json) ->
    true.
