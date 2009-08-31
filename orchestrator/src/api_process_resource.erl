-module(api_process_resource).

-export([init/1, allowed_methods/2, resource_exists/2, content_types_provided/2]).
-export([process_post/2]).

-export([status_json/2]).

-include("webmachine.hrl").
-include("api.hrl").

-record(state, {kind, status_doc}).

init([{kind, Kind}]) ->
    {ok, #state{kind=Kind, status_doc=undefined}}.

allowed_methods(Req, State) ->
    {['GET', 'HEAD', 'POST'], Req, State}.

content_types_provided(Req, State) ->
    {[{"application/json", status_json},
      {"text/plain", status_json}],
     Req, State}.

resource_exists(Req, State = #state{kind=Kind}) ->
    Id = wrq:path_info(id, Req),
    StatusDocType = api_util:status_doc_type(Kind),
    case streams:status_doc(Id) of
        {ok, Doc} ->
            case rfc4627:get_field(Doc, "type") of
                {ok, StatusDocType} -> {true, Req, State};
                _ -> {false, Req, State}
            end;
        _ -> {false, Req, State}
    end.

process_post(Req, State = #state{kind=Kind}) ->
    Id = wrq:path_info(id, Req),
    case rfc4627_jsonrpc_webmachine:json_rpc(rpc_service_rec(Kind, Id), Req) of
        {result, Result} ->
            {true, wrq:append_to_response_body(rfc4627:encode(Result), Req), State};
        {result, Result, _} ->
            {true, wrq:append_to_response_body(rfc4627:encode(Result), Req), State};
        {error, Error} -> % JSON-RPC errors are returned as 500
            {{error, rfc4627:encode(Error)}, Req, State}
    end.

status_json(Req, State = #state{kind=Kind}) ->
    Id = wrq:path_info(id, Req),
    {rfc4627:encode(status(Kind, Id)), Req, State}.

% ------

status(ResourceType, ThingId) ->
    {obj,
     [{api_util:url(process, ResourceType, ThingId),
       streams:process_status(list_to_binary(ThingId))}]}.

rpc_service_rec(Kind, Id) ->
    rfc4627_jsonrpc:service(
      {function, process_control(Kind, Id)},
      "process-control",
      "urn:uuid:8596d670-9662-11de-8a39-0800200c9a66", %% should this be the endpoint URL?
      "0.3", % use streams version?
      [rfc4627_jsonrpc:proc("start", []),
       rfc4627_jsonrpc:proc("stop", [])]).

process_control(Kind, Id) ->
    fun (Method, _, []) ->
            case Method of
                <<"start">> ->
                    streams:activate(Kind, Id);
                <<"stop">> ->
                    streams:deactivate(Kind, Id)
            end,
            {result, {obj, [{url, list_to_binary(api_util:url(process, Kind, Id))}]}}
    end.
