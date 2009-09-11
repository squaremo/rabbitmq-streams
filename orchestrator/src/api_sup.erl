%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc Supervisor for the api application.

-module(api_sup).
-author('Michael Bridgen <mikeb@lshift.net>').

-behaviour(supervisor).

%% External exports
-export([start_link/2, upgrade/0]).

%% supervisor callbacks
-export([init/1]).

%% @spec start_link(integer(), string()) -> ServerRet
%% @doc API for starting the supervisor.
start_link(Port, LogDir) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [Port, LogDir]).

%% @spec upgrade() -> ok
%% @doc Add processes if necessary.
upgrade() ->
    {ok, {_, Specs}} = init([]),

    Old = sets:from_list(
            [Name || {Name, _, _, _} <- supervisor:which_children(?MODULE)]),
    New = sets:from_list([Name || {Name, _, _, _, _, _} <- Specs]),
    Kill = sets:subtract(Old, New),

    sets:fold(fun (Id, ok) ->
                      supervisor:terminate_child(?MODULE, Id),
                      supervisor:delete_child(?MODULE, Id),
                      ok
              end, ok, Kill),

    [supervisor:start_child(?MODULE, Spec) || Spec <- Specs],
    ok.

%% @spec init([]) -> SupervisorTree
%% @doc supervisor callback.
init([Port, LogDir]) ->
    Ip = case os:getenv("MOCHIWEB_IP") of false -> "0.0.0.0"; Any -> Any end,   
    {ok, Dispatch} = file:consult(
                       filename:join([orchestrator:priv_dir(), "dispatch.conf"])),
    WebConfig = [{ip, Ip},
                 {port, Port}, %% FIXME use application env
                 {log_dir, LogDir}, %% FIXME use application env
                 {dispatch, Dispatch}],
    Web = {webmachine_mochiweb,
           {webmachine_mochiweb, start, [WebConfig]},
           permanent, 5000, worker, dynamic},
    Processes = [Web],
    {ok, {{one_for_one, 10, 10}, Processes}}.
