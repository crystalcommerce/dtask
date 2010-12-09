-module(dtask_config_tests).

-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

dtask_config_test_() ->
    {setup,
     fun setup_tmp_dir/0,
     fun cleanup_tmp_dir/1,
     {foreach,
      fun get_directory/0,
      fun clear_directory/1,
      [
       ?_test(dtask_config_loads_options_from_file()),
       ?_test(dtask_config_returns_empty_config_if_theres_no_file()),
       ?_test(get_returns_undefined_if_it_doesnt_have_the_option())
      ]}}.

dtask_config_loads_options_from_file() ->
    create_sample_config(),
    Config = dtask_config:new(),
    ?assertMatch("value", dtask_config:get(opt_one, Config)),
    ?assertMatch("value two", dtask_config:get(opt_two, Config)).

dtask_config_returns_empty_config_if_theres_no_file() ->
    Config = dtask_config:new(),
    ?assertMatch(0, length(Config)).

get_returns_undefined_if_it_doesnt_have_the_option() ->
    create_sample_config(),
    Config = dtask_config:new(),
    ?assertMatch(undefined, dtask_config:get(not_real, Config)).
    
setup_tmp_dir() ->
    ok = file:make_dir("tmp"),
    ok = file:set_cwd("tmp"),
    get_directory().

cleanup_tmp_dir(Dir) ->
    ok = file:set_cwd(Dir),
    clear_directory(Dir),
    ok = file:set_cwd(".."),
    ok = file:del_dir(Dir).

get_directory() ->
    {ok, Dir} = file:get_cwd(),
    Dir.

clear_directory(Directory) ->
    {ok, Files} = file:list_dir(Directory),
    lists:foreach(fun(File) ->
                          ok = file:delete(File)
                  end, Files).

create_sample_config() ->
    file:write_file("dtask.config",
                    "{opt_one, \"value\"}.\n" ++
                        "{opt_two, \"value two\"}.").
