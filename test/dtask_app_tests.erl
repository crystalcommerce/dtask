-module(dtask_app_tests).

-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

generate_application_test_() ->
    {setup,
     fun start_app/0,
     fun stop_app/1,
     [
      ?_test(dtask_sup_is_running()),
      ?_test(dtask_is_running()),
      ?_test(dtask_timer_is_running())
     ]}.

dtask_sup_is_running() ->
    ?assertNot(undefined == whereis(dtask_sup)).

dtask_is_running() ->
    ?assertNot(undefined == whereis(dtask_srv)).

dtask_timer_is_running() ->
    ?assertNot(undefined == whereis(dtask_timer)).

start_app() ->
    Dir = dtask_test_util:setup_tmp_dir(),
    dtask_test_util:create_sample_config([node()]),
    ok = application:start(dtask),
    Dir.
    
stop_app(TmpDir) ->
    ok = application:stop(dtask),
    dtask_test_util:cleanup_tmp_dir(TmpDir).
