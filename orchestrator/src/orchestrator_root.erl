-module(orchestrator_root).

-behaviour(gen_server).

-export([start_link/0]).
-export([open_channel/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-define(ROOT_STATUS_DOCID, ?FEEDSHUB_STATUS_DBNAME "root_config").

-include("orchestrator.hrl").
-include("rabbit.hrl").
-include("rabbit_framing.hrl").

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

open_channel() ->
    gen_server:call(?SERVER, open_channel).

%%---------------------------------------------------------------------------

-record(root_config, {rabbitmq_host, rabbitmq_admin_user, rabbitmq_admin_password}).

setup_core_messaging(Ch, LogCh) ->
    #'exchange.declare_ok'{} =
        amqp_channel:call(LogCh, #'exchange.declare'{exchange = ?FEEDSHUB_LOG_XNAME,
                                                  type = <<"topic">>,
                                                  durable = true}),
    #'exchange.declare_ok'{} =
        amqp_channel:call(Ch, #'exchange.declare'{exchange = ?FEEDSHUB_CONFIG_XNAME,
                                                  type = <<"topic">>,
                                                  durable = false}),
    PrivateQ = lib_amqp:declare_private_queue(Ch),
    #'queue.bind_ok'{} = lib_amqp:bind_queue(Ch, ?FEEDSHUB_CONFIG_XNAME, PrivateQ, <<"*">>),
    _ConsumerTag = lib_amqp:subscribe(Ch, PrivateQ, self()),
    ok.

setup_logger(LogCh) ->
    case supervisor:start_child(orchestrator_root_sup,
    			       {orchestrator_root_logger,
    				{orchestrator_logger, start_link, [LogCh]},
    				permanent,
    				brutal_kill,
    				worker,
    				[orchestrator_logger]
    			       }) of
	{ok, _Pid} -> ok;
	{error, {already_started, _Pid}} -> ok
    end.


install_views() ->
    lists:foreach(
      fun({WC, DB}) ->
	      lists:foreach(fun(Dir) -> install_view(DB, Dir) end,
			    filelib:wildcard(orchestrator:priv_dir() ++ WC))
      end, [
	    {"/feedshub_status/views/*", ?FEEDSHUB_STATUS_DBNAME}
	   ]),
    ok.

install_view(DbName, ViewDir) ->
    ViewCollectionName = filename:basename(ViewDir),
    Views = lists:foldl(fun (FileName, V) ->
                                Base = filename:basename(FileName, ".js"),
                                Extn = filename:extension(Base),
                                ViewName = filename:basename(Base, Extn),
                                "." ++ FunctionName = Extn,
                                {ok, FunctionText} = file:read_file(FileName),
                                dict:update(ViewName,
                                            fun (OldViewDict) ->
                                                    dict:store(FunctionName,
                                                               FunctionText,
                                                               OldViewDict)
                                            end,
                                            dict:store(FunctionName, FunctionText, dict:new()),
                                            V)
                        end, dict:new(), filelib:wildcard(ViewDir++"/*.*.js")),
    Path = DbName ++ "_design/" ++ ViewCollectionName,
    Doc = {obj, [{"views", Views},
		 {"language", <<"javascript">>}]},
    Doc2 =
	case couchapi:get(Path) of
	     {ok, InstalledViews} -> {ok, RevId} = rfc4627:get_field(InstalledViews, "_rev"),
				     rfc4627:set_field(Doc, "_rev", RevId);
	     _ -> Doc
	 end,
    {ok, _} = couchapi:put(Path, Doc2).

setup_core_couch() ->
    ok = couchapi:createdb(?FEEDSHUB_STATUS_DBNAME),
    {ok, _} = couchapi:put(?ROOT_STATUS_DOCID,
                           {obj, [{"feedshub_version", ?FEEDSHUB_VERSION},
                                  {"rabbitmq", {obj, [{"host", <<"localhost">>},
                                                      {"user", <<"feedshub_admin">>},
                                                      {"password", <<"feedshub_admin">>}]}}
                                 ]}),
    ok = install_views(),
    ok.

read_root_config() ->
    {ok, RootConfig} = couchapi:get(?ROOT_STATUS_DOCID),
    case rfc4627:get_field(RootConfig, "feedshub_version") of
        {ok, ?FEEDSHUB_VERSION} ->
            {ok, RMQ} = rfc4627:get_field(RootConfig, "rabbitmq"),
            {ok, RHost} = rfc4627:get_field(RMQ, "host"),
            {ok, RUser} = rfc4627:get_field(RMQ, "user"),
            {ok, RPassword} = rfc4627:get_field(RMQ, "password"),
            {ok, #root_config{rabbitmq_host = binary_to_list(RHost),
                              rabbitmq_admin_user = binary_to_list(RUser),
                              rabbitmq_admin_password = binary_to_list(RPassword)}};
        {ok, Other} ->
            exit({feedshub_version_mismatch, [{expected, ?FEEDSHUB_VERSION},
                                              {detected, Other}]})
    end.

startup_couch_scan() ->
    {ok, CouchInfo} = couchapi:get(""),
    {couchdb_presence_check, {ok, _}} = {couchdb_presence_check,
                                         rfc4627:get_field(CouchInfo, "couchdb")},
    {couchdb_version_check, {ok, <<"0.9.0">>}} = {couchdb_version_check,
                                                  rfc4627:get_field(CouchInfo, "version")},
    case couchapi:get(?FEEDSHUB_STATUS_DBNAME) of
        {ok, _DbInfo} ->
            ok;
        {error, 404, _} ->
            ok = setup_core_couch()
    end,
    {ok, #root_config{}} = read_root_config().

activate_thing(ThingId, Module, Args) when is_binary(ThingId) ->
    case supervisor:start_child(orchestrator_root_sup,
                                {ThingId,
                                 {Module, start_link, [Args]},
                                 transient,
                                 5000,
                                 supervisor,
                                 [orchestrator_feed_sup]}) of
        {ok, _ChildPid} ->
            ok;
        {error, {already_started, _Child}} ->
            ok;
        {error, {shutdown, _}} ->
            error_logger:error_report({?MODULE, activate_thing, start_error, {ThingId, Module, Args}}),
            {error, start_error}
    end;
activate_thing(ThingId, Module, Args) ->
    activate_thing(list_to_binary(ThingId), Module, Args).

deactivate_thing(ThingId) when is_binary(ThingId) ->
    Res =
	case supervisor:terminate_child(orchestrator_root_sup, ThingId) of
	    ok ->
		ok;
	    {error, not_found} ->
		ok;
	    {error, simple_one_for_one} ->
		error_logger:error_report({?MODULE, deactivate_thing, supervision_config_error, ThingId}),
		{error, supervision_config_error}
	end,
    case Res of
	ok ->
	    case supervisor:delete_child(orchestrator_root_sup, ThingId) of
		ok ->
		    ok;
		{error, Err} ->
		    error_logger:error_report({?MODULE, deactivate_feed, Err, ThingId})
	    end;
	_ -> Res
    end;
deactivate_thing(ThingId) ->
    deactivate_thing(list_to_binary(ThingId)).

activate_server(ServerId, Args) ->
    activate_thing(ServerId, orchestrator_server_sup, Args).

deactivate_server(ServerId, _Args) ->
    deactivate_thing(ServerId).

check_active_servers(Channel, Connection) ->
    ServerIds = [lists:takewhile(fun (C) -> C /= $_ end,
				 binary_to_list(rfc4627:get_field(R, "id", undefined)))
		 || R <- couchapi:get_view_rows(?FEEDSHUB_STATUS_DBNAME, "servers", "active")],
    lists:foreach(fun(ServerId) -> activate_server(ServerId, [ServerId,
							      Channel, Connection,
							      Channel, Connection,
							      Channel, Connection]) end, ServerIds),
    ok.

activate_feed(FeedId) ->
    activate_thing(FeedId, orchestrator_feed_sup, [FeedId]).

deactivate_feed(FeedId) ->
    deactivate_thing(FeedId).

check_active_feeds() ->
    FeedIds = [lists:takewhile(fun (C) -> C /= $_ end,
			       binary_to_list(rfc4627:get_field(R, "id", undefined)))
               || R <- couchapi:get_view_rows(?FEEDSHUB_STATUS_DBNAME, "feeds", "active")],
    lists:foreach(fun activate_feed/1, FeedIds),
    ok.

activate_terminal(TermId, Channel) when is_binary(TermId) ->
    case orchestrator_server:find_server_for_terminal(TermId) of
	{ok, ServerId} ->
	    Props = (amqp_util:basic_properties()) #'P_basic' { delivery_mode = 2 },
	    lib_amqp:publish(Channel, ?FEEDSHUB_CONFIG_XNAME,
			     list_to_binary(ServerId ++ "." ++ TermId),
			     <<"status change">>, Props);
	Err ->
	    error_logger:error_report({?MODULE, thing_terminal, Err, TermId})
    end;
activate_terminal(TermId, Channel) ->
    activate_terminal(list_to_binary(TermId), Channel).

check_active_terminals(Channel) ->
    TermIds =
	[lists:takewhile(fun (C) -> C /= $_ end,
			  binary_to_list(rfc4627:get_field(R, "id", undefined)))
	 || R <- couchapi:get_view_rows(?FEEDSHUB_STATUS_DBNAME, "terminals", "active")],
    lists:foreach(fun (TermId) -> activate_terminal(TermId, Channel) end, TermIds),
    ok.
    
status_change(ThingId, Channel, Connection) when is_binary(ThingId) ->
    case couchapi:get(?FEEDSHUB_STATUS_DBNAME ++ binary_to_list(ThingId) ++ "_status") of
	{ok, Doc} ->
	    {On, Off, Args} =
		case rfc4627:get_field(Doc, "type") of
		    %% Terminals should never show up here as they
		    %% should not match the routing key of our queue
		    %% bound to the config exchange, and should be
		    %% picked up directly by the server.
		    {ok, <<"feed-status">>} ->
			{fun activate_feed/1, fun deactivate_feed/1, [ThingId]};
		    {ok, <<"server-status">>} ->
			{fun activate_server/2, fun deactivate_server/2, [ThingId, [ThingId,
										    Channel, Connection,
										    Channel, Connection,
										    Channel, Connection]]};
		    Err ->
			error_logger:error_report({?MODULE, status_change, Err, ThingId})
		end,
	    case rfc4627:get_field(Doc, "active") of
		{ok, true} ->
	    	    apply(On, Args);
	     	{ok, false} ->
	     	    apply(Off, Args);
	     	Err2 ->
	     	    error_logger:error_report({?MODULE, status_change, Err2, ThingId})
	    end;
	Err3 ->
	    error_logger:error_report({?MODULE, status_change, Err3, ThingId})
    end.

%%---------------------------------------------------------------------------

-record(state, {config, amqp_connection, ch, logger_ch}).

init([]) ->
    {ok, Configuration = #root_config{rabbitmq_host = RHost,
                                      rabbitmq_admin_user = RUser,
                                      rabbitmq_admin_password = RPassword}}
        = startup_couch_scan(),
    AmqpConnectionPid = amqp_connection:start_link(RUser, RPassword, RHost),
    Ch = amqp_connection:open_channel(AmqpConnectionPid),
    LogCh = amqp_connection:open_channel(AmqpConnectionPid),
    ok = setup_core_messaging(Ch, LogCh),
    gen_server:cast(self(), setup_logger), %% do this after we're fully running.
    gen_server:cast(self(), check_active_things), %% do this after we're fully running.
    {ok, #state{config = Configuration,
                amqp_connection = AmqpConnectionPid,
                ch = Ch,
		logger_ch = LogCh
	       }}.

handle_call(open_channel, _From, State = #state{amqp_connection = Conn}) ->
    {reply, {ok, amqp_connection:open_channel(Conn)}, State};
handle_call(_Message, _From, State) ->
    {stop, unhandled_call, State}.

handle_cast(setup_logger, State = #state {logger_ch = LogCh}) ->
    ok = setup_logger(LogCh),
    {noreply, State};
handle_cast(check_active_things, State = #state { ch = Ch, amqp_connection = Connection }) ->
    ok = check_active_servers(Ch, Connection),
    ok = check_active_terminals(Ch),
    ok = check_active_feeds(),
    {noreply, State};
handle_cast(_Message, State) ->
    {stop, unhandled_cast, State}.

handle_info(#'basic.consume_ok'{}, State) ->
    %% As part of setup_core_messaging, we subscribe to a few
    %% things. Ignore the success notices.
    {noreply, State};
handle_info({#'basic.deliver' { exchange = ?FEEDSHUB_CONFIG_XNAME,
				'delivery_tag' = DeliveryTag,
				'routing_key' = RoutingKey
			       },
	     #content { payload_fragments_rev = [<<"status change">>]}},
	    State = #state{ch = Ch, amqp_connection = Connection}) ->
    status_change(RoutingKey, Ch, Connection),
    lib_amqp:ack(Ch, DeliveryTag),
    {noreply, State};
handle_info({#'basic.deliver' { exchange = ?FEEDSHUB_CONFIG_XNAME,
				'delivery_tag' = DeliveryTag
			       },
	     #content { payload_fragments_rev = [<<"install views">>]}},
	    State = #state{ch = Ch}) ->
    ok = install_views(),
    lib_amqp:ack(Ch, DeliveryTag),
    {noreply, State};
handle_info(_Info, State) ->
    {stop, unhandled_info, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
