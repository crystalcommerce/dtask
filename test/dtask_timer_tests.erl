-module(dtask_timer_tests).

-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

dtask_timer_test_() ->
    {setup, 
     fun start_dtask/0, 
     fun stop_dtask/1, 
     {foreach,
      fun start_dtask_timer/0,
      fun stop_dtask_timer/1,
      [
       ?_test(schedule_creates_task()),
       ?_test(schedule_schedules_task()),
       ?_test(cancel_returns_error_with_invalid_tref()),
       ?_test(cancel_removes_the_task())
      ]}
    }.

schedule_creates_task() ->
    {ok, TRef} = dtask_timer:schedule(5000, io, format, ["Test", []]),
    % This returns an error if the task doesn't exist
    ?assertMatch({ok, cancel}, dtask_timer:cancel(TRef)).

schedule_schedules_task() ->
    {ok, _TRef} = dtask_timer:schedule(100, ?MODULE, ping, [self()]),
    ?assertMatch(ok, receive
                         ping -> ok
                     after
                         100 ->  timeout
                     end).

cancel_returns_error_with_invalid_tref() ->
    ?assertMatch({error, badarg}, dtask_timer:cancel(88)).

cancel_removes_the_task() ->
    {ok, TRef} = dtask_timer:schedule(5000, io, format, ["Test", []]),
    {ok, LeaveAlone} = dtask_timer:schedule(5000, io, format, ["AnotherTest", []]),
    {ok, cancel} = dtask_timer:cancel(TRef),
    {ok, cancel} = dtask_timer:cancel(LeaveAlone),
    ?assertMatch({error, badarg}, dtask_timer:cancel(TRef)).

start_dtask_timer() ->
    {ok, Pid} = dtask_timer:start_link(),
    Pid.

stop_dtask_timer(Pid) ->
    gen_server:call(Pid, stop).

start_dtask() ->
    {ok, Pid2} = dtask_srv:start_link(),
    dtask_srv:register(node()),
    Pid2.

stop_dtask(Pid) ->
    gen_server:call(Pid, stop).

ping(Node) ->
    Node ! ping.
