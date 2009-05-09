-module(shovel).

-behaviour(gen_server).

-export([start_link/4]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([bind_source_to_exchange/3, unbind_source_from_exchange/2]).

-include("rabbit.hrl").
-include("rabbit_framing.hrl").

-record(source_config, { connection, queue }).
-record(destination_config, { connection, exchange }).
-record(state, { source_config, source_channel, destination_config, destination_channel, rk_map }).

start_link(SourceConnection, SourceQueue, RemoteConnection, RemoteExchange) ->
    gen_server:start_link(?MODULE,
			  [#source_config { connection = SourceConnection, queue = SourceQueue },
			   #destination_config { connection = RemoteConnection, exchange = RemoteExchange }],
			  []).

bind_source_to_exchange(Shovel, {X, BKIn}, RKOut) ->
    gen_server:cast(Shovel, {bind_source, {X, BKIn}, RKOut}).

unbind_source_from_exchange(Shovel, {X, BKIn}) ->
    gen_server:cast(Shovel, {unbind_source, {X, BKIn}}).

init([SourceConfig = #source_config { connection = SourceConnection,
				      queue = SourceQueue },
      DestinationConfig = #destination_config { connection = DestinationConnection }]) ->

    SourceCh = amqp_connection:open_channel(SourceConnection),
    DestinationCh = amqp_connection:open_channel(DestinationConnection),

    SourceQueue2 = case SourceQueue of
		       undefined -> lib_amqp:declare_private_queue(SourceCh);
		       _ -> SourceQueue
		   end,
    
    _ConsumerTag = lib_amqp:subscribe(SourceCh, SourceQueue2, self()),

    RKMap = ets:new(list_to_atom(pid_to_list(self())), [set, private]),

    {ok, #state { source_config = SourceConfig #source_config { queue = SourceQueue2 },
		  source_channel = SourceCh,
		  destination_config = DestinationConfig,
		  destination_channel = DestinationCh,
		  rk_map = RKMap
		}
    }.

handle_call(_Message, _From, State) ->
    {stop, unhandled_call, State}.

handle_cast({bind_source, {X, BKIn}, RKOut},
	    State = #state { source_channel = SourceCh,
			     source_config = #source_config { queue = SourceQ },
			     rk_map = RKMap
			   }) ->
    true = ets:insert(RKMap, {X, RKOut}),
    #'queue.bind_ok'{} = lib_amqp:bind_queue(SourceCh, X, SourceQ, BKIn),
    {noreply, State};
    
handle_cast({unbind_source, {X, BKIn}},
	    State = #state { source_channel = SourceCh,
			     source_config = #source_config { queue = SourceQ },
			     rk_map = RKMap
			   }) ->
    #'queue.unbind_ok'{} = lib_amqp:unbind_queue(SourceCh, X, SourceQ, BKIn),
    true = ets:delete(RKMap, X),
    {noreply, State};
    
handle_cast(_Message, State) ->
    {stop, unhandled_cast, State}.

handle_info(#'basic.consume_ok'{}, State) ->
    {noreply, State};
handle_info({#'basic.deliver' { 'delivery_tag' = DeliveryTag,
				exchange = XIn,
				'routing_key' = RKIn
			      },
	     Content},
	    State = #state { source_channel = SourceCh,
			     destination_channel = DestinationCh,
			     destination_config = #destination_config { exchange = DestinationExchange },
			     rk_map = RKMap
			    }) ->
    [{XIn, RKOut}] = ets:lookup(RKMap, XIn),
    RKOut2 = if RKOut =:= keep -> RKIn;
		true -> RKOut
	     end,
    BasicPublish = #'basic.publish'{exchange = DestinationExchange,
                                    routing_key = RKOut2
				   },
    amqp_channel:cast(DestinationCh, BasicPublish, Content),
    lib_amqp:ack(SourceCh, DeliveryTag),
    {noreply, State};
handle_info(_Info, State) ->
    {stop, unhandled_info, State}.

terminate(_Reason, #state { source_channel = SourceCh,
			    destination_channel = DestinationCh
			   }) ->
    lib_amqp:close_channel(SourceCh),
    lib_amqp:close_channel(DestinationCh),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
