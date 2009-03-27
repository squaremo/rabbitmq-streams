-module(orchestrator_feed).

-behaviour(gen_server).

-export([start_link/1]).
-export([selfcheck/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("orchestrator.hrl").
-include("rabbit_framing.hrl").

start_link(FeedId) ->
    gen_server:start_link(?MODULE, [FeedId], []).

selfcheck(FeedPid) ->
    ok = gen_server:cast(FeedPid, selfcheck).

%%---------------------------------------------------------------------------

-record(state, {feed_id, running_components}).

do_selfcheck(State = #state{feed_id = FeedId,
                            running_components = OldRunningComponents}) ->
    {ok, FeedDefinition} = couchapi:get(?FEEDSHUB_CONFIG_DBNAME ++ binary_to_list(FeedId)),
    error_logger:info_report({?MODULE, selfcheck, FeedDefinition}),
    State.

%%---------------------------------------------------------------------------

init([FeedId]) ->
    selfcheck(self()),
    {ok, #state{feed_id = FeedId,
                running_components = dict:new()}}.

handle_call(_Message, _From, State) ->
    {stop, unhandled_call, State}.

handle_cast(selfcheck, State) ->
    {noreply, do_selfcheck(State)};
handle_cast(_Message, State) ->
    {stop, unhandled_cast, State}.

handle_info(_Info, State) ->
    {stop, unhandled_info, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
