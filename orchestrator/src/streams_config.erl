-module(streams_config).

-export([check_config/0, config_doc_url/0, state_server/0, config_db/0]).

%% The following are unsafe (i.e., may blow up if the env values are
%% not found).

%% @doc Return a list of cnofig items missing.
check_config() ->
    lists:filter(fun (X) ->
                         case application:get_env(orchestrator, X) of
                             {ok, Val} ->
                                 false;
                             _ -> true
                         end
                 end,
                 [root_config_url, couch_base_url, config_db]).

unsafe_env_value(Key) ->
    {ok, Val} = application:get_env(orchestrator, Key),
    Val.

config_doc_url() -> unsafe_env_value(root_config_url).
state_server() -> unsafe_env_value(couch_base_url).
config_db() -> unsafe_env_value(config_db).
