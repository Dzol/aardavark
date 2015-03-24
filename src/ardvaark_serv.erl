-module(ardvaark_serv).
-behaviour(gen_server).

-export([start_link/1,
	 stop/0,
	 message_list/0,
	 message_count/0]).

-export([init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3]).

-include_lib("amqp_client/include/amqp_client.hrl").
-include_lib("eunit/include/eunit.hrl").

-record(state, {con, chan, messages = [], count = 0}).

%% ===================================================================
%% API
%% ===================================================================

start_link(Queue) when is_binary(Queue)->
    gen_server:start_link({local, ardvaark_serv}, ardvaark_serv, [Queue], []).

stop() ->
    gen_server:cast(ardvaark_serv, stop),
    ok.

message_list() ->
    {message_list, Messages} = gen_server:call(ardvaark_serv, message_list),
    {message_list, Messages}.

message_count() ->
    {message_count, Total} = gen_server:call(ardvaark_serv, message_count),
    {message_count, Total}.

%% ===================================================================
%% Generic server callback procedures
%% ===================================================================

init([Queue]) ->
    {Con, Chan} = connect_to_queue("localhost", Queue),
    amqp_channel:subscribe(Chan,
			   #'basic.consume'{queue = Queue,
					    no_ack = true}, self()),
    {ok, #state{con = Con, chan = Chan, messages = []}}.

handle_call(message_list, _Sender, State) ->
    {reply, {message_list, State#state.messages}, State};
handle_call(message_count, _Sender, State) ->
    {reply, {message_count, State#state.count}, State}.

handle_cast(listen, _State) ->
    {noreply, _State};
handle_cast(stop, _State) ->
    % unlink at some point?
    {stop, normal, _State}.

handle_info(#'basic.consume_ok'{}, _State) ->
    {noreply, _State};
handle_info({#'basic.deliver'{}, #amqp_msg{payload = Body}}, State) ->
    io:format(" [OK] Received ~p~n", [Body]),
    Messages = [Body|State#state.messages],
    Count = State#state.count + 1,
    {noreply, State#state{messages = Messages, count = Count}};
handle_info(Message, State) ->
    io:format("unexpected info: ~p.", [Message]),
    {noreply, State}.

terminate(_Reason, State) ->
    Con = State#state.con,
    Chan =State#state.chan,
    ok = disconnect_from_queue({Con, Chan}),
    ok.

code_change(_Old, _State, _Extra) ->
    {error, not_supported}.

%% ===================================================================
%% Auxiliary procedures
%% ===================================================================

connect_to_queue(Host, Queue) when is_binary(Queue) ->
    {ok, Con} = amqp_connection:start(
		  #amqp_params_network{host = Host}),
    {ok, Chan} = amqp_connection:open_channel(Con),
    amqp_channel:call(Chan, #'queue.declare'{queue = Queue}),
    {Con, Chan}.

disconnect_from_queue({Con, Chan}) ->
    ok = amqp_channel:close(Chan),
    ok = amqp_connection:close(Con),
    ok.

