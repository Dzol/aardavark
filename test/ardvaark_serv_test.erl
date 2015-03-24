-module(ardvaark_serv_test).

-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("amqp_client/include/amqp_client.hrl").

-compile(export_all).

%% ===================================================================
%% Test procedures
%% ===================================================================

% Just a single process

run_complete_test(Host, Queue, Message, Count) ->
    {Con, Chan} = connect_to_queue(Host, Queue),
    PID = ardvaark_serv:start_link(Host, Queue),
    send_messages(Chan, Queue, Message, Count),
    Count = ardvaark_serv:message_count(PID),
    ok = disconnect_from_queue({Con, Chan}),
    ardvaark_serv:stop(PID). 

zero_messages_test() ->
    run_complete_test("localhost", <<"zero messages">>, <<"ping">>, 0).

one_message_test() ->
    run_complete_test("localhost", <<"one message">>, <<"ping">>, 1).

two_message_test() ->
    run_complete_test("localhost", <<"two messages">>, <<"ping">>, 2).

% Multiple processes

two_ardvaarks_test() ->

    Total = 3200,
    Host = "localhost",
    Queue = <<"two ardvaarks">>,
    Message = <<"ping">>,

    {Con, Chan} = connect_to_queue(Host, Queue),
    ok = send_messages(Chan, Queue, Message, Total),
    ok = disconnect_from_queue({Con, Chan}),

    PIDOne = ardvaark_serv:start_link(Host, Queue),
    PIDTwo = ardvaark_serv:start_link(Host, Queue),
    ct:pal("Ardvaark PIDs: ~p, ~p.~n", [PIDOne, PIDTwo]),

    PartOne = ardvaark_serv:message_count(PIDOne),
    PartTwo = ardvaark_serv:message_count(PIDTwo),
    ct:pal("Parts: ~p, ~p.~n", [PartOne, PartTwo]),
    Total = PartOne + PartTwo,

    ok = ardvaark_serv:stop(PIDOne),
    ok = ardvaark_serv:stop(PIDTwo),
    
    ok.

% Exometer tests

exometer_metrics_test() ->

    Total = 3200,
    Host = "localhost",
    Queue = <<"exometer metrics">>,
    Message = <<"ping">>,    

    {ok, _Started} = application:ensure_all_started(exometer),

    {Con, Chan} = connect_to_queue(Host, Queue),
    ok = send_messages(Chan, Queue, Message, Total),
    ok = disconnect_from_queue({Con, Chan}),

    PID = ardvaark_serv:start_link(Host, Queue),
    timer:sleep(500),

    Info = exometer:info(ardvaark_serv:exometer_name(PID)),
    ct:pal("Info = ~p~n", [Info]),
    {ok, Data} = exometer:get_value(ardvaark_serv:exometer_name(PID)),
    ct:pal("Data = ~p~n", [Data]),

    {count, 3200} = lists:keyfind(count, 1, Data),

    ok = ardvaark_serv:stop(PID).

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
