%% @author Michael Bridgen <mikeb@lshift.net>
%% @copyright 2009 Michael Bridgen, LShift Ltd.

%% @doc Web server for Streams API.

-module(api_web).
-author('Michael Bridgen <mikeb@lshift.net>').

-export([start/1, stop/0, loop/2]).

-include("api.hrl").

%% External API

start(Options) ->
    {DocRoot, Options1} = get_option(docroot, Options),
    Loop = fun (Req) ->
                   ?MODULE:loop(Req, DocRoot)
           end,
    mochiweb_http:start([{name, ?MODULE}, {loop, Loop} | Options1]).

stop() ->
    mochiweb_http:stop(?MODULE).

%% Dispatch to static resource or facet
loop(Req, DocRoot) ->
    Path = Req:get(path),
    Method = (case Req:get(method) of
                 'GET' -> getorhead;
                 'HEAD' -> getorhead;
                 'POST' -> post;
                 'DELETE' -> delete;
                 'PUT' -> put
             end),
    case re:split(Path, "/", [{parts, 4}]) of
        [<<>>, <<"static">> | _] ->
            handle_static(Path, DocRoot, Req, Method);
        [<<>>, <<>>] ->
            handle_root(DocRoot, Req, Method);
        [<<>>, Facet, ResourceType, Name] ->
            case check_resource_type(ResourceType) of
                {ok, ResourceTypeAtom} ->
                    handle_method(ResourceTypeAtom,
                                  binary_to_list(Facet),
                                  Name,
                                  Req,
                                  Method);
                {error, invalid_resource_type} ->
                    Req:not_found()
            end;
        _ ->
            Req:not_found()
    end.

%% Internal API

% Supplied in skeleton
get_option(Option, Options) ->
    {proplists:get_value(Option, Options), proplists:delete(Option, Options)}.

json_response(Req, Code, JsonStructure) ->
    % Content negoitation here?
    Req:respond({Code,
                 [{"Content-Type", "application/json"}],
                 rfc4627:encode(JsonStructure)}).

check_resource_type(<<"terminal">>) -> {ok, terminal};
check_resource_type(<<"pipeline">>) -> {ok, pipeline};
check_resource_type(_) -> {error, invalid_resource_type}.

% Adapted from RabbitHub
handle_static("/" ++ StaticFile, DocRoot, Req, getorhead) ->
    Req:serve_file(StaticFile, DocRoot);
handle_static(_OtherPath, _DocRoot, Req, getorhead) ->
    Req:respond({400, [], "Invalid path"});
handle_static(_, _, Req, _) ->
    Req:respond({405, [{"Allow", "GET, HEAD"}], "Method not allowed"}).

% TODO Server status
handle_root(DocRoot, Req, getorhead) ->
    json_response(Req, 200, app_status());
handle_root(_, Req, _) ->
    Req:respond({405, [{"Allow", "GET, HEAD"}], "Method not allowed"}).

%% dispatch to particular facet and resource

%% Handle index requests
handle_method(ResourceTypeAtom, Facet, <<>>, Req, getorhead) ->
    handle_index(ResourceTypeAtom, Facet, Req);

% This won't be the case when one can create by POSTing
handle_method(_, _, <<>>, Req, _) ->
    Req:response({405, [{"Allow", "GET, HEAD"}], "Method not allowed"});

handle_method(pipeline, "process", Id, Req, getorhead) ->
    case thing_process_status(pipeline, Id) of
        {notfound, _} -> Req:response({404, [], "Not found."});
        Pair -> json_response(Req, 200, {obj, [Pair]})
    end;

%% Cover all, but note we still want explicit method-not-alloweds above
handle_method(_, _, _, Req, _) ->
    Req:respond({404, [], "Method call not known."}).

handle_index(pipeline, "model", Req) ->
    json_response(Req, 200, list_pipelines());
handle_index(ResourceTypeAtom, Facet, Req) ->
    Req:respond({404, [], "Not found."}).

app_status() ->
    {obj, [{"application", ?APPLICATION_NAME},
           {"version", ?APPLICATION_VERSION}]}.

list_pipelines() ->
    {obj, [pipeline_pair(P) || P <- streams:all_pipelines(streams_config:config_db())]}.


% -------------------------------------------------

pipeline_pair(Row) ->
    {ok, Id} = rfc4627:get_field(Row, "key"),
    {ok, PipelineDesc} = rfc4627:get_field(Row, "value"),
    PipelineUrl = api_url(model, pipeline, binary_to_list(Id)),
    {PipelineUrl, PipelineDesc}.

thing_process_status(ResourceType, ThingId) ->
    StatusDocType = status_doc_type(ResourceType),
    case streams:status_doc(ThingId) of
        {ok, Doc} ->
            case rfc4627:get_field(Doc, "type") of
                {ok, StatusDocType}  ->
                    {api_url(process, ResourceType, binary_to_list(ThingId)),
                     streams:process_status(ThingId)};
                _ -> {notfound, wrongtype}
            end;
        _ -> {notfound, nomodelitem}
    end.

status_doc_type(pipeline) -> <<"feed-status">>.

% TODO Check the facet and resource type
api_url(Facet, ResourceType, Id) ->
    "/" ++ mochiweb_util:join([atom_to_list(Facet), atom_to_list(ResourceType), Id], "/").
