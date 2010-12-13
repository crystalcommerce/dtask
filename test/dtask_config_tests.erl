-module(dtask_config_tests).

-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

dtask_config_test_() ->
    {setup,
     fun dtask_test_util:setup_tmp_dir/0,
     fun teardown/1,
     {foreach,
      fun dtask_test_util:get_directory/0,
      fun dtask_test_util:clear_directory/1,
      [
       ?_test(dtask_config_loads_options_from_dtask_config_file()),
       ?_test(dtask_config_returns_empty_config_if_theres_no_file()),
       ?_test(dtask_config_loads_from_config_env_option_if_set()),
       ?_test(get_returns_undefined_if_it_doesnt_have_the_option())
      ]}}.

dtask_config_loads_options_from_dtask_config_file() ->
    create_sample_config("dtask.config"),
    Config = dtask_config:new(),
    ?assertMatch("value", dtask_config:get(opt_one, Config)),
    ?assertMatch("value two", dtask_config:get(opt_two, Config)).

dtask_config_loads_from_config_env_option_if_set() ->
    create_sample_config("other_file.config"),
    application:set_env(dtask, config, "other_file.config"),
    Config = dtask_config:new(),
    ?assertMatch("value", dtask_config:get(opt_one, Config)),
    ?assertMatch("value two", dtask_config:get(opt_two, Config)).

dtask_config_returns_empty_config_if_theres_no_file() ->
    Config = dtask_config:new(),
    ?assertMatch(0, length(Config)).

get_returns_undefined_if_it_doesnt_have_the_option() ->
    create_sample_config("dtask.config"),
    Config = dtask_config:new(),
    ?assertMatch(undefined, dtask_config:get(not_real, Config)).

create_sample_config(Path) ->
    file:write_file(Path,
                    "{opt_one, \"value\"}.\n" ++
                        "{opt_two, \"value two\"}.").

teardown(Arg) ->
    dtask_test_util:cleanup_tmp_dir(Arg),
    application:unset_env(dtask, config).
