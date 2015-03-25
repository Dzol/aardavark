-module(ardvaark_serv).
-behaviour(gen_server).

-export([start_link/2,
	 stop/1,
	 message_list/1,
	 message_count/1,
	 exometer_name/1]).

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

start_link(Host, Queue) when is_binary(Queue) ->
    {ok, PID} = gen_server:start_link(?MODULE, [Host, Queue], []),
    PID.

stop(PID) ->
    gen_server:cast(PID, stop),
    ok.

message_list(PID) ->
    gen_server:call(PID, message_list).

message_count(PID) ->
    gen_server:call(PID, message_count).

exometer_name(PID) ->
    [message, count, list_to_atom(pid_to_list(PID))].

%% ===================================================================
%% Generic server callback procedures
%% ===================================================================

init([Host, Queue]) ->

    {ok, _Started} = application:ensure_all_started(exometer),
    exometer:new(exometer_name(self()), spiral),
%    exometer:setopts(exometer_name(self()), [{slot_period, 500}]),

    {Con, Chan} = connect_to_queue(Host, Queue),
    amqp_channel:subscribe(Chan,
			   #'basic.consume'{queue = Queue,
					    no_ack = true}, self()),
    {ok, #state{con = Con, chan = Chan, messages = []}}.

handle_call(message_list, _Sender, State) ->
    {reply, State#state.messages, State};
handle_call(message_count, _Sender, State) ->
    {reply, State#state.count, State}.

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
    exometer:update(exometer_name(self()), 1),
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

