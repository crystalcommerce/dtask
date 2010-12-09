-module(dtask_test_util).

-export([
         setup_tmp_dir/0,
         cleanup_tmp_dir/1,
         get_directory/0,
         create_sample_config/1,
         clear_directory/1
        ]).

setup_tmp_dir() ->
    ok = file:make_dir("tmp"),
    ok = file:set_cwd("tmp"),
    get_directory().

cleanup_tmp_dir(Dir) ->
    ok = file:set_cwd(Dir),
    clear_directory(Dir),
    ok = file:set_cwd(".."),
    ok = file:del_dir(Dir).

clear_directory(Directory) ->
    {ok, Files} = file:list_dir(Directory),
    lists:foreach(fun(File) ->
                          ok = file:delete(File)
                  end, Files).

get_directory() ->
    {ok, Dir} = file:get_cwd(),
    Dir.

create_sample_config(Nodes) ->
    NodeStr = string:join(lists:map(fun atom_to_list/1, Nodes), "', '"),
    file:write_file("dtask.config",
                    "{nodes, ['" ++ NodeStr ++ "']}.").
