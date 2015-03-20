-module(ardvaark_serv).
-behaviour(gen_server).

-export([start_link/0]).

-export([init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3]).

start_link() ->
    gen_server:start_link({local, ardvaark_serv}, ardvaark_serv, [], []).

init(_Args) ->
    Count = 0,
    {ok, Count}.

handle_call(count_request, _Sender, Count) ->
    {reply, {count_response, Count}, Count}.

handle_cast(ping, Count) ->
    {noreply, 1 + Count}.

handle_info(Info, _State) ->
    {stop, Info, []}.

terminate(_Reason, _State) ->
    ok.

code_change(_Old, _State, _Extra) ->
    {error, not_supported}.
