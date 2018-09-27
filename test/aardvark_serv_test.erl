-module(aardvark_serv_test).

-include_lib("eunit/include/eunit.hrl").
-include_lib("amqp_client/include/amqp_client.hrl").

% To-Do list:
% (0) coordinate with rabbit, e.g. purge queues in case something goes wrong
% (1) get rid of redundant procedures, and reorganise them into correct sections
% (2) set Host, Queue, Message, etc somewhere else instead of in partial procedures?


%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% TESTS DESCRIPTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%

start_stop_and_link_test_() ->
    {"Many aardvark servers can be linked to the parent proccess.",
     [{setup,
       fun () -> start_aardvarks(Number) end,
       fun stop_aardvarks/1,
       fun linked_to_all/1
      } || Number <- [1, 2, 16]
     ]
    }.

one_server_message_count_test_() ->
    {"A single aardvark server can count 0, 1, and 2 messages.",
     [{setup,
       fun () ->
	       queue_messages(Number),
	       start_aardvark() end,
       fun stop_aardvark/1,
       fun (PID) ->
	       [?_assert(Number == message_count(PID))] end
      } || Number <- [0, 1, 2]
     ]
    }.

eight_aardvark_s_message_total_test_() ->
    {"Eight aardvark servers can count a correct total of all 2048 queued messages.",
     {setup,
      fun () ->
	      queue_messages(2048),
	      start_aardvarks(8) end,
      fun stop_aardvarks/1,
      fun (PIDs) when is_list(PIDs) ->
	      timer:sleep(5 * 1000),
	      [?_assertEqual(2048,  message_total(PIDs))] end
     }
    }.

eight_arvaark_s_exometer_total_test_() ->
    {"Eight aardvark servers can count a correct exometer total of all 2048 queued messages.",
     {setup,
      fun () ->
	      start_exometer(),
	      queue_messages(2048),
	      start_aardvarks(8) end,
      fun stop_aardvarks/1,
      fun (PIDs) when is_list(PIDs) ->
	      timer:sleep(5 * 1000),
	      [?_assertEqual(2048,  exometer_total(PIDs))] end
     }
    }.


%%%%%%%%%%%%%%%%%%%%
%%% ACTUAL TESTS %%%
%%%%%%%%%%%%%%%%%%%%

message_count(PID) ->
    aardvark_serv:message_count(PID).

message_total(PIDs) when is_list(PIDs) ->
    Counts = lists:map(fun message_count/1, PIDs),
    lists:sum(Counts).

linked_to(PID) when is_pid(PID) ->
    {links, ListOfPIDs} = process_info(self(), links),
    SetOfPIDs = sets:from_list(ListOfPIDs),
    [?_assert(true = sets:is_element(PID, SetOfPIDs))].

linked_to_all(PIDs) when is_list(PIDs) ->
    lists:map(fun linked_to/1, PIDs).

exometer_count(PID) ->

    {ok, Data} = exometer:get_value(
		   aardvark_serv:exometer_name(PID)),
    {count, Val} = lists:keyfind(count, 1, Data),
    Val.

exometer_total(PIDs) when is_list(PIDs) ->

    Counts = lists:map(fun exometer_count/1, PIDs),
    lists:sum(Counts).


%%%%%%%%%%%%%%%%%%%%%%%
%%% SETUP FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%

send_messages_to_queue(Host, Queue, Message, Total) ->

    {Con, Chan} = connect_to_queue(Host, Queue),
    ok = send_messages(Chan, Queue, Message, Total),
    ok = disconnect_from_queue({Con, Chan}).

start_aardvarks(Num) when Num > 0 ->

    lists:map(fun (_Arg) -> aardvark_serv:start_link("localhost", <<"test queue">>) end, lists:seq(1, Num)).

stop_aardvarks(PIDs) ->

    lists:map(fun (PID) -> ok = aardvark_serv:stop(PID) end, PIDs),
    ok.

start_aardvark() ->
    aardvark_serv:start_link("localhost", <<"test queue">>).

stop_aardvark(PID) ->
    aardvark_serv:stop(PID).

queue_messages(Number) ->
    send_messages_to_queue("localhost", <<"test queue">>, <<"test message">>, Number).

start_exometer() ->
    {ok, _Started} = application:ensure_all_started(exometer),
    ok.


%%%%%%%%%%%%%%%%%%%%%%%%
%%% HELPER FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%

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
