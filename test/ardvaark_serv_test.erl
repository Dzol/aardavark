-module(ardvaark_serv_test).

-include_lib("eunit/include/eunit.hrl").
-include_lib("amqp_client/include/amqp_client.hrl").

-compile(export_all).

%% ===================================================================
%% Test procedures
%% ===================================================================

run_complete_test(Host, Queue, Message, Count) ->
    {Con, Chan} = connect_to_queue(Host, Queue),
    ardvaark_serv:start_link(Queue),
    send_messages(Chan, Queue, Message, Count),
    {message_count, Count} = ardvaark_serv:message_count(),
    ok = disconnect_from_queue({Con, Chan}),
    ardvaark_serv:stop(). 

zero_messages_test() ->
    run_complete_test("localhost", <<"zero messages">>, <<"ping">>, 0).

one_message_test() ->
    run_complete_test("localhost", <<"one message">>, <<"ping">>, 1).

two_message_test() ->
    run_complete_test("localhost", <<"two messages">>, <<"ping">>, 2).

%% ===================================================================
%% Auxiliary procedures
%% ===================================================================

%% -spec connect_to_queue(Place :: string(), Name :: binary()) ->
%% 			 {Con :: connection(), Chan :: channel()}.

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

send_messages(Chan, Queue, Message, Count) when Count > 0 ->
    ok = amqp_channel:cast(Chan,
			   #'basic.publish'{
			      exchange = <<"">>, % default exchange
			      routing_key = Queue},
			   #amqp_msg{payload = Message}),
    send_messages(Chan, Queue, Message, Count - 1);
send_messages(_Chan, _Queue, _Message, 0) ->
    timer:sleep(500),
    ok.
