%% @author Michael Bridgen <mikeb@lshift.net>
%% @copyright 2009 Michael Bridgen, LShift Ltd.

%% @doc Web server for Streams API.

-module(api_web).
-author('Michael Bridgen <mikeb@lshift.net>').

-export([start/1, stop/0, loop/2]).

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
    case re:split(Path, "/", [{parts, 4}]) of
        [<<>>, <<"static">> | _] ->
            handle_static(Path, DocRoot, Req);
        [<<>>, <<>>] ->
            handle_root(DocRoot, Req);
        [<<>>, Facet, ResourceType, Name] ->
            case check_resource_type(ResourceType) of
                {ok, ResourceTypeAtom} ->
                    handle_method(ResourceTypeAtom,
                                  binary_to_list(Facet),
                                  Name,
                                  Req);
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

check_resource_type(<<"terminal">>) -> {ok, terminal};
check_resource_type(<<"pipeline">>) -> {ok, pipeline};
check_resource_type(_) -> {error, invalid_resource_type}.

% Adapted from RabbitHub
handle_static("/" ++ StaticFile, DocRoot, Req) ->
    case Req:get(method) of
        Method when Method =:= 'GET'; Method =:= 'HEAD' ->
            Req:serve_file(StaticFile, DocRoot);
        _ -> Req:respond({405, [{"Allow", "GET, HEAD"}], "Method not allowed"})
    end;
handle_static(_OtherPath, _DocRoot, Req) ->
    Req:respond({400, [], "Invalid path"}).

% TODO Server status
handle_root(DocRoot, Req) ->
    Req:respond({200, [], "Streams API"}).

% dispatch to particular facet and resource
handle_method(ResourceTypeAtom, Facet, <<>>, Req) ->
    handle_index(ResourceTypeAtom, Facet, Req);    
handle_method(ResourceTypeAtom, Facet, Name, Req) ->
    Req:respond({200, [], "Method call."}).

handle_index(ResourceTypeAtom, Facet, Req) ->
    Req:ok({200, [], "Index."}).
