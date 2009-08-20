-module(orchestrator).

-export([start/0, stop/0, start/2, stop/1]).
-export([restart/0, stop_and_halt/0, status/0]).

-export([priv_dir/0]).

start() -> application:start(?MODULE).
stop() -> application:stop(?MODULE).

start(normal, []) ->
    %% Report this if not empty
    [] = streams_config:check_config(),
    print_banner(),
    ok = api_deps:ensure(),
    {ok, _} = ibrowse_sup:start_link(),
    orchestrator_root_sup:start_link().

stop(_State) ->
    ok.

stop_and_halt() ->
    spawn(fun () ->
                  SleepTime = 1000,
                  error_logger:info_msg("Stop-and-halt request received; "
                                        "halting in ~p milliseconds~n",
                                        [SleepTime]),
                  timer:sleep(SleepTime),
                  init:stop()
          end),
    case catch stop() of _ -> ok end.

status() ->
    [{running_applications, application:which_applications()}].

restart() ->
    orchestrator_root:install_views(),
    stop(),
    start().

priv_dir() ->
    case code:priv_dir(?MODULE) of
        {error, bad_name} ->
            {ok, Cwd} = file:get_cwd(),
            Cwd ++ "/priv";
        D ->
            D
    end.

print_banner() ->
    {ok, Product} = application:get_key(id),
    {ok, Version} = application:get_key(vsn),
    io:format("~s ~s~n", [Product, Version]),
    Settings = [{"node", node()},
                {"state server", streams_config:state_server()},
                {"config URL", streams_config:config_doc_url()},
                {"config database", streams_config:config_db()}],
    DescrLen = lists:max([length(K) || {K, _} <- Settings]),
    Format = "~-" ++ integer_to_list(DescrLen) ++ "s: ~s~n",
    lists:foreach(fun ({K, V}) -> io:format(Format, [K, V]) end, Settings),
    io:nl().
