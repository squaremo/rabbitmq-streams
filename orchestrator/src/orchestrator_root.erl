-module(orchestrator_root).

-behaviour(gen_server).

-export([start_link/0]).
-export([open_channel/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-define(ROOT_CONFIG_DOCID, ?FEEDSHUB_CONFIG_DBNAME "root_config").

-include("orchestrator.hrl").
-include("rabbit.hrl").
-include("rabbit_framing.hrl").

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

open_channel() ->
    gen_server:call(?SERVER, open_channel).

%%---------------------------------------------------------------------------

-record(root_config, {rabbitmq_host, rabbitmq_admin_user, rabbitmq_admin_password}).

setup_core_messaging(Ch) ->
    #'exchange.declare_ok'{} =
        amqp_channel:call(Ch, #'exchange.declare'{exchange = ?FEEDSHUB_CONFIG_XNAME,
                                                  type = <<"topic">>,
                                                  durable = true}),
    PrivateQ = lib_amqp:declare_private_queue(Ch),
    #'queue.bind_ok'{} = lib_amqp:bind_queue(Ch, ?FEEDSHUB_CONFIG_XNAME, PrivateQ, <<"#">>),
    _ConsumerTag = lib_amqp:subscribe(Ch, PrivateQ, self()),
    ok.

install_views() ->
    lists:foreach(
      fun({WC, DB}) ->
	      lists:foreach(fun(Dir) -> install_view(DB, Dir) end,
			    filelib:wildcard(orchestrator:priv_dir() ++ WC))
      end, [{"/feedshub_config/views/*", ?FEEDSHUB_CONFIG_DBNAME},
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
    {ok, _} = couchapi:put(DbName ++ "_design/" ++ ViewCollectionName,
                           {obj, [{"views", Views},
                                  {"language", <<"javascript">>}]}).

setup_core_couch() ->
    ok = couchapi:createdb(?FEEDSHUB_CONFIG_DBNAME),
    ok = couchapi:createdb(?FEEDSHUB_STATUS_DBNAME),
    {ok, _} = couchapi:put(?ROOT_CONFIG_DOCID,
                           {obj, [{"feedshub_version", ?FEEDSHUB_VERSION},
                                  {"rabbitmq", {obj, [{"host", <<"localhost">>},
                                                      {"user", <<"feedshub_admin">>},
                                                      {"password", <<"feedshub_admin">>}]}}
                                 ]}),
    ok = install_views(),
    ok.

read_root_config() ->
    {ok, RootConfig} = couchapi:get(?ROOT_CONFIG_DOCID),
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
    case couchapi:get(?FEEDSHUB_CONFIG_DBNAME) of
        {ok, _DbInfo} ->
            ok;
        {error, 404, _} ->
            ok = setup_core_couch()
    end,
    {ok, #root_config{}} = read_root_config().

check_active_feeds() ->
    FeedIds = [rfc4627:get_field(R, "id", undefined)
               || R <- couchapi:get_view_rows(?FEEDSHUB_STATUS_DBNAME, "feeds", "active")],
    lists:foreach(fun activate_feed/1, FeedIds),
    ok.

activate_feed(FeedId) ->
    case supervisor:start_child(orchestrator_root_sup,
                                {FeedId,
                                 {orchestrator_feed_sup, start_link, [FeedId]},
                                 transient,
                                 5000,
                                 supervisor,
                                 [orchestrator_feed_sup]}) of
        {ok, _ChildPid} ->
            ok;
        {error, {already_started, _Child}} ->
            ok;
        {error, {shutdown, _}} ->
            error_logger:error_report({?MODULE, activate_feed, start_error, FeedId}),
            {error, start_error}
    end.

deactivate_feed(FeedId) ->
    Res =
	case supervisor:terminate_child(orchestrator_root_sup,
					FeedId) of
	    ok ->
		ok;
	    {error, not_found} ->
		ok;
	    {error, simple_one_for_one} ->
		error_logger:error_report({?MODULE, deactivate_feed, supervision_config_error, FeedId}),
		{error, supervision_config_error}
	end,
    case Res of
	ok ->
	    case supervisor:delete_child(orchestrator_root_sup, FeedId) of
		ok ->
		    ok;
		{error, Err} ->
		    error_logger:error_report({?MODULE, deactivate_feed, Err, FeedId})
	    end;
	_ -> Res
    end.

status_change(FeedId) when is_binary(FeedId) ->
    case couchapi:get(?FEEDSHUB_STATUS_DBNAME ++ binary_to_list(FeedId)) of
	{ok, Doc} ->
	    case rfc4627:get_field(Doc, "active") of
		{ok, true} ->
	    	    activate_feed(FeedId);
	     	{ok, false} ->
	     	    deactivate_feed(FeedId);
	     	Err ->
	     	    error_logger:error_report({?MODULE, status_change, Err, FeedId})
	    end;
	{error, DecodeError} ->
	    error_logger:error_report({?MODULE, status_change, DecodeError, FeedId})
    end.

%%---------------------------------------------------------------------------

-record(state, {config, amqp_connection, ch}).

init([]) ->
    {ok, Configuration = #root_config{rabbitmq_host = RHost,
                                      rabbitmq_admin_user = RUser,
                                      rabbitmq_admin_password = RPassword}}
        = startup_couch_scan(),
    AmqpConnectionPid = amqp_connection:start_link(RUser, RPassword, RHost),
    Ch = amqp_connection:open_channel(AmqpConnectionPid),
    ok = setup_core_messaging(Ch),
    gen_server:cast(self(), check_active_feeds), %% do this after we're fully running.
    {ok, #state{config = Configuration,
                amqp_connection = AmqpConnectionPid,
                ch = Ch}}.

handle_call(open_channel, _From, State = #state{amqp_connection = Conn}) ->
    {reply, {ok, amqp_connection:open_channel(Conn)}, State};
handle_call(_Message, _From, State) ->
    {stop, unhandled_call, State}.

handle_cast(check_active_feeds, State) ->
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
	    State = #state{ch = Ch}) ->
    status_change(RoutingKey),
    lib_amqp:ack(Ch, DeliveryTag),
    {noreply, State};
handle_info(_Info, State) ->
    {stop, unhandled_info, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
