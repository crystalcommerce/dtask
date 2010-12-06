-module(dtask_node_list_tests).

-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

node_list_test_() ->
    [
     ?_test(focus_on_empty_list_is_undefined()),
     ?_test(new_with_empty_list_returns_empty_node_list()),
     ?_test(new_sets_focus_to_first_element_of_list()),
     ?_test(step_shifts_focus_to_next_element()),
     ?_test(step_while_focus_on_last_returns_focus_to_start()),
     ?_test(size_returns_zero_with_no_nodes()),
     ?_test(size_returns_the_number_of_nodes()),
     ?_test(add_increases_the_size_of_the_list()),
     ?_test(seek_returns_list_with_focus_on_element()),
     ?_test(seek_returns_an_error_if_the_element_isnt_found()),
     ?_test(seek_returns_an_error_on_empty_lists()),
     ?_test(drop_removes_the_current_element()),
     ?_test(step_returns_self_on_empty_list()),
     ?_test(adding_to_empty_list_focuses_new_element())
    ].

focus_on_empty_list_is_undefined() ->
    ?assertMatch(undefined, dtask_node_list:focus(dtask_node_list:new([]))).

new_with_empty_list_returns_empty_node_list() ->
    ?assert(dtask_node_list:is_empty(dtask_node_list:new([]))).

new_sets_focus_to_first_element_of_list() ->
    ?assertMatch(a, dtask_node_list:focus(dtask_node_list:new([a, b, c]))).

step_shifts_focus_to_next_element() ->
    List = dtask_node_list:step(dtask_node_list:new([a, b, c])),
    ?assertMatch(b, dtask_node_list:focus(List)).

step_while_focus_on_last_returns_focus_to_start() ->
    Second = dtask_node_list:step(dtask_node_list:new([a, b, c])),
    Third = dtask_node_list:step(Second),
    First = dtask_node_list:step(Third),
    ?assertMatch(a, dtask_node_list:focus(First)).

step_returns_self_on_empty_list() ->
    List = dtask_node_list:new([]),
    ?assertMatch(List, dtask_node_list:step(List)).

size_returns_zero_with_no_nodes() ->
    ?assertMatch(0, dtask_node_list:size(dtask_node_list:new([]))).

size_returns_the_number_of_nodes() ->
    ?assertMatch(3, dtask_node_list:size(dtask_node_list:new([a, b, c]))).

add_increases_the_size_of_the_list() ->
    Original = dtask_node_list:new([a, b, c]),
    ?assertMatch(4, dtask_node_list:size(dtask_node_list:add(d, Original))).

adding_to_empty_list_focuses_new_element() ->
    List = dtask_node_list:add(a, dtask_node_list:new([])),
    ?assertMatch(a, dtask_node_list:focus(List)).

seek_returns_list_with_focus_on_element() ->
    {ok, New} = dtask_node_list:seek(c, dtask_node_list:new([a, b, c, d])),
    ?assertMatch(c, dtask_node_list:focus(New)).

seek_returns_an_error_if_the_element_isnt_found() ->
    ?assertMatch({error, not_found},
                 dtask_node_list:seek(z, dtask_node_list:new([a, b, c]))).

seek_returns_an_error_on_empty_lists() ->
    ?assertMatch({error, not_found},
                 dtask_node_list:seek(z, dtask_node_list:new([]))).

drop_removes_the_current_element() ->
    Original = dtask_node_list:new([a, b, c, d]),
    Expected = dtask_node_list:new([b, c, d]),
    ?assertMatch(Expected, dtask_node_list:drop(Original)).
