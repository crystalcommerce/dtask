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
     ?_test(adding_to_empty_list_focuses_new_element()),
     ?_test(drop_wraps_to_first_element_if_on_last()),
     ?_test(remove_maintains_the_current_focus()),
     ?_test(remove_removes_the_element()),
     ?_test(remove_focus_sets_focus_to_next_element()),
     ?_test(remove_returns_error_if_element_not_found()),
     ?_test(removing_the_last_element_makes_a_list_empty()),
     ?_test(difference_removes_all_elements_from_provided_list()),
     ?_test(difference_removes_subset_if_not_all_present()),
     ?_test(difference_with_empty_list_returns_original()),
     ?_test(difference_with_empty_original_returns_empty()),
     ?_test(concat_adds_all_elements()),
     ?_test(concat_with_empty_list_returns_original()),
     ?_test(concat_with_empty_original_returns_other())
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

drop_wraps_to_first_element_if_on_last() ->
    Second = dtask_node_list:step(dtask_node_list:new([a, b, c])),
    Third = dtask_node_list:step(Second),
    First = dtask_node_list:drop(Third),
    ?assertMatch(a, dtask_node_list:focus(First)).

remove_maintains_the_current_focus() ->
    List  = dtask_node_list:new([a, b, c, d]),
    List2 = dtask_node_list:remove(c, List),
    ?assertMatch(a, dtask_node_list:focus(List2)).

remove_removes_the_element() ->
    List  = dtask_node_list:new([a, b, c, d]),
    List2 = dtask_node_list:remove(d, List),
    ?assertMatch({error, not_found}, dtask_node_list:seek(d, List2)).

remove_focus_sets_focus_to_next_element() ->
    List  = dtask_node_list:new([a, b, c, d]),
    List2 = dtask_node_list:remove(a, List),
    ?assertMatch(b, dtask_node_list:focus(List2)).

remove_returns_error_if_element_not_found() ->
    List = dtask_node_list:new([]),
    ?assertMatch({error, not_found}, dtask_node_list:remove(a, List)).

removing_the_last_element_makes_a_list_empty() ->
    List  = dtask_node_list:new([a]),
    List2 = dtask_node_list:remove(a, List),
    ?assert(dtask_node_list:is_empty(List2)).

difference_removes_all_elements_from_provided_list() ->
    List1 = dtask_node_list:new([a, b, c, d]),
    List2 = dtask_node_list:new([b, d]),
    Result = dtask_node_list:difference(List1, List2),
    ?assertMatch({error, not_found}, dtask_node_list:seek(b, Result)),
    ?assertMatch({error, not_found}, dtask_node_list:seek(d, Result)),
    ?assertMatch({ok, _}, dtask_node_list:seek(a, Result)),
    ?assertMatch({ok, _}, dtask_node_list:seek(c, Result)).

difference_removes_subset_if_not_all_present() ->
    List1 = dtask_node_list:new([a, b, c]),
    List2 = dtask_node_list:new([b, d]),
    Result = dtask_node_list:difference(List1, List2),
    ?assertMatch({error, not_found}, dtask_node_list:seek(b, Result)),
    ?assertMatch({ok, _}, dtask_node_list:seek(a, Result)),
    ?assertMatch({ok, _}, dtask_node_list:seek(c, Result)).

difference_with_empty_list_returns_original() ->
    List1 = dtask_node_list:new([a, b, c ,d]),
    List2 = dtask_node_list:new([]),
    ?assertMatch(List1, dtask_node_list:difference(List1, List2)).

difference_with_empty_original_returns_empty() ->
    List1 = dtask_node_list:new([]),
    List2 = dtask_node_list:new([a, b, c ,d]),
    ?assertMatch(List1, dtask_node_list:difference(List1, List2)).

concat_adds_all_elements() ->
    List1 = dtask_node_list:new([a, b]),
    List2 = dtask_node_list:new([c, d]),
    Result = dtask_node_list:concat(List1, List2),
    ?assertMatch({ok, _}, dtask_node_list:seek(a, Result)),
    ?assertMatch({ok, _}, dtask_node_list:seek(b, Result)),
    ?assertMatch({ok, _}, dtask_node_list:seek(c, Result)),
    ?assertMatch({ok, _}, dtask_node_list:seek(d, Result)).

concat_with_empty_list_returns_original() ->
    List1 = dtask_node_list:new([a, b, c ,d]),
    List2 = dtask_node_list:new([]),
    ?assertMatch(List1, dtask_node_list:concat(List1, List2)).

concat_with_empty_original_returns_other() ->
    List1 = dtask_node_list:new([]),
    List2 = dtask_node_list:new([a, b, c ,d]),
    ?assertMatch(List2, dtask_node_list:concat(List1, List2)).
