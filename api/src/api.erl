%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc TEMPLATE.

-module(api).
-author('author <author@example.com>').
-export([start/0, stop/0]).

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.
        
%% @spec start() -> ok
%% @doc Start the api server.
start() ->
    api_deps:ensure(),
    ensure_started(crypto),
    application:start(api).

%% @spec stop() -> ok
%% @doc Stop the api server.
stop() ->
    Res = application:stop(api),
    application:stop(crypto),
    Res.
