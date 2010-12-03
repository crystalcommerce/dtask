-module(dtask_app_tests).

-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

generate_application_test_() ->
    {setup,
     fun start_app/0,
     fun stop_app/1,
     [
      ?_test(dtask_sup_is_running()),
      ?_test(dtask_timer_is_running())
     ]}.

dtask_sup_is_running() ->
    ?assertNot(undefined == whereis(dtask_sup)).

dtask_timer_is_running() ->
    ?assertNot(undefined == global:whereis_name(dtask_timer)).

start_app() ->
    ok = application:start(dtask).
    
stop_app(_Arg) ->
    ok = application:stop(dtask).
