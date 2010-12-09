-module(dtask_sup_tests).

-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

sup_test_() ->
    {setup,
     fun setup/0,
     fun dtask_test_util:cleanup_tmp_dir/1,
     [
      ?_test(init_loads_candidates_from_config())
     ]}.

init_loads_candidates_from_config() ->
    Expected = ['node1@192.168.1.9', 'node2@192.168.1.100'],
    {ok, {{_, _, _}, Children}} = dtask_sup:init([]),
    lists:foreach(fun(ChildSpec) ->
                          ?assertMatch(Expected, get_init_args(ChildSpec))
                  end, Children).

get_init_args({_, {_, _, [Args]}, _, _, _, _}) ->
    Args.

setup() ->
    Dir = dtask_test_util:setup_tmp_dir(),
    dtask_test_util:create_sample_config(['node1@192.168.1.9',
                                          'node2@192.168.1.100']),
    Dir.
