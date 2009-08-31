%% JSON (RFC 4627) RPC adapter for webmachine
%%--------------------------------------------------------------------------------
%% @author Michael Bridgen <mikeb@lshift.net>
%%
%% @reference <a href="">Erlang RFC 4627</a>
%% @reference <a href="">WebMachine</a>
%%
%% @doc This is an adapter to erlang-rfc4627's JSON-RPC implementation of webmachine's
%% processing model.  I have left out the registering and lookup of service records;
%% see rfc4627_jsonrpc_mochiweb.erl for that kind of thing.
%%
%% Use {@link json_rpc/2} from, for example, your resource's <code>process_post/2</code>,
%% like so:
%%
%% <pre class="code erlang">
%% process_post(Req, State) ->
%%   case json_rpc(rpc_service_rec(Req), Req) of
%%     {result, Result, _} ->
%%       {true, wrq:append_to_response_body(rfc4627:encode(Result), Req), State};
%%     {error, Reason} ->
%%       {{error, rfc4627:encode(Reason)}, Req, state}
%%   end.
%% </pre>
%% 
%% <code>rpc_service_rec/1</code> is some procedure htat constructs a
%% rfc4627_jsonrpc service().

-module(rfc4627_jsonrpc_webmachine).

-export([json_rpc/2]).

json_rpc(ServiceRec, Req) ->
    case rfc4627:decode(wrq:req_body(Req)) of
        {ok, Obj, []} ->
            Timeout = extract_timeout(Req),
            rfc4627_jsonrpc:jsonrpc_post(ServiceRec, undefined, Obj, Timeout);
        {error, Reason} ->
            rfc4627_jsonrpc:error_response(500, "Unable to parse request body: " ++ Reason)
    end.

extract_timeout(Req) ->
    case wrq:get_req_header("x-json-rpc-timeout", Req) of
        undefined -> default;
        "default" -> default;
        "infinity" -> infinity;
        Other -> list_to_integer(Other)
    end.

