-module(orchestrator_terminal_logger).

-behaviour(gen_event).

-export([init/1, handle_event/2, handle_call/2, handle_info/2, code_change/3, terminate/2]).

-include("rabbit.hrl").
-include("rabbit_framing.hrl").

init([Exchanges, RoutingKey, Channel]) ->
    PrivateQ = lib_amqp:declare_private_queue(Channel),
    lists:foreach(
      fun (Exchange) ->
              #'queue.bind_ok'{} = lib_amqp:bind_queue(Channel, Exchange, PrivateQ, list_to_binary(RoutingKey)),
              _ConsumerTag = lib_amqp:subscribe(Channel, PrivateQ, self(), false)
      end,
      Exchanges),
    {ok, Channel}.

handle_event(_ErrorMsg, State) ->
    {stop, unhandled_event, State}.

handle_call(_Request, State) ->
    {stop, unhandled_call, State}.

handle_info(#'basic.consume_ok'{}, State) ->
    %% As part of setup_core_messaging, we subscribe to a few
    %% things. Ignore the success notices.
    {ok, State};
handle_info({#'basic.deliver' { 'delivery_tag' = DeliveryTag,
				'routing_key' = RoutingKey
			      },
	     #content { payload_fragments_rev = MessageRev }},
	    Channel) ->
    Message = lists:concat(lists:map(fun erlang:binary_to_list/1, lists:reverse(MessageRev))),
    [Level|RKRest] =
	lists:foldr(fun (Char, [Cur|Rest]) ->
			    if Char == $. -> [[],Cur|Rest];
			       true -> [[Char|Cur]|Rest]
			    end;
			(Char, []) ->
			    if Char == $. -> [];
			       true -> [[Char]]
			    end
		    end, [], binary_to_list(RoutingKey)),
    RKFormatted = " | " ++ lists:flatmap(fun (T) -> T ++ " | " end, RKRest),
    case Level of
	"debug" -> error_logger:info_msg("*** DEBUG ***~n~s~n ~s~n", [RKFormatted, Message]);
	"info"  -> error_logger:info_msg("~s~n ~s~n", [RKFormatted, Message]);
	"warn"  -> error_logger:warning_msg("~s~n ~s~n", [RKFormatted, Message]);
	"error" -> error_logger:error_msg("~s~n ~s~n", [RKFormatted, Message]);
	"fatal" -> error_logger:error_msg("*** FATAL ***~n~s~n ~s~n", [RKFormatted, Message]);
	Notify  -> error_logger:info_msg("*** Notification ***~n~s~n ~s: ~s~n", [RKFormatted, Notify, Message])
    end,
    lib_amqp:ack(Channel, DeliveryTag),
    {ok, Channel};
handle_info(_Info, State) -> 
    {stop, unhandled_info, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Args, _State) ->
    ok.
