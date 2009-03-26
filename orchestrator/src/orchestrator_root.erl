-module(orchestrator_root).

-behaviour(gen_server).

-export([start_link/0]).
-export([open_channel/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-include("orchestrator.hrl").
-include("rabbit_framing.hrl").

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

open_channel() ->
    gen_server:call(?SERVER, open_channel).

%%---------------------------------------------------------------------------

setup_core_resources(Ch) ->
    #'exchange.declare_ok'{} =
        amqp_channel:call(Ch, #'exchange.declare'{exchange = ?FEEDSHUB_CONFIG_XNAME,
                                                  type = <<"topic">>,
                                                  durable = true}),
    PrivateQ = lib_amqp:declare_private_queue(Ch),
    #'queue.bind_ok'{} = lib_amqp:bind_queue(Ch, ?FEEDSHUB_CONFIG_XNAME, PrivateQ, <<"#">>),
    _ConsumerTag = lib_amqp:subscribe(Ch, PrivateQ, self()),
    ok.

startup_couch_scan() ->
    ok.

%%---------------------------------------------------------------------------

-record(state, {amqp_connection, ch}).

init([]) ->
    {ok, RabbitHost} = application:get_env(rabbitmq_host),
    {ok, Username} = application:get_env(rabbitmq_feedshub_admin_user),
    {ok, Password} = application:get_env(rabbitmq_feedshub_admin_password),
    AmqpConnectionPid = amqp_connection:start_link(Username, Password, RabbitHost),
    Ch = amqp_connection:open_channel(AmqpConnectionPid),
    ok = setup_core_resources(Ch),
    ok = startup_couch_scan(),
    {ok, #state{amqp_connection = AmqpConnectionPid,
                ch = Ch}}.

handle_call(open_channel, _From, State = #state{amqp_connection = Conn}) ->
    {reply, {ok, amqp_connection:open_channel(Conn)}, State};
handle_call(_Message, _From, State) ->
    {stop, unhandled_call, State}.

handle_cast(_Message, State) ->
    {stop, unhandled_cast, State}.

handle_info(#'basic.consume_ok'{}, State) ->
    %% As part of setup_core_resources, we subscribe to a few
    %% things. Ignore the success notices.
    {noreply, State};
handle_info(_Info, State) ->
    {stop, unhandled_info, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
