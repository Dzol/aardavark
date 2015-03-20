-module(ardvaark_app_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

application_up_test() ->
    ok = application:ensure_started(ardvaark).

%% application_ping_test() ->
%%    ok = gen_server:cast(ardvaark_serv, ping).

%% application_count_test() ->
%%    {count_response, 1} = gen_server:call(ardvaark_serv, count_request).
