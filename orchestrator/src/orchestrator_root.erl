-module(orchestrator_root).

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%---------------------------------------------------------------------------

-record(state, {amqp_connection}).

init([]) ->
    {ok, RabbitHost} = application:get_env(rabbitmq_host),
    {ok, Username} = application:get_env(rabbitmq_feedshub_admin_user),
    {ok, Password} = application:get_env(rabbitmq_feedshub_admin_password),
    AmqpConnectionPid = amqp_connection:start_link(Username, Password, RabbitHost),
    {ok, #state{amqp_connection = AmqpConnectionPid}}.

handle_call(_Message, _From, State) ->
    {stop, unhandled_call, State}.

handle_cast(_Message, State) ->
    {stop, unhandled_cast, State}.

handle_info(_Info, State) ->
    {stop, unhandled_info, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
