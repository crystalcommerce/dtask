%%%--------------------------------------------------------------------------
%%% @author Ryan Burrows <rhburrows@gmail.com>
%%% @doc
%%%  DTask Node List is a zipper implementation on a list. It is in concept a
%%%  circuarly-linked list with a pointer to a current "focus" element. Calling
%%%  step advances this pointer through the list.
%%%
%%%  Elements are ordered but the order of additional elements being inserted
%%%  is not guaranteed.
%%% @end
%%%--------------------------------------------------------------------------
-module(dtask_node_list).

% API
-export([new/1, is_empty/1, focus/1, step/1, size/1, add/2,
         seek/2, drop/1]).

-type node_list() :: {list(), list()}.

%%%==========================================================================
%%% API
%%%==========================================================================

%%---------------------------------------------------------------------------
%% @doc
%%  Creates a new node list based off of the given list of elements. The focus
%%  will be set to the first element.
%%
%% @end
%%---------------------------------------------------------------------------
-spec new(list()) -> node_list().
new(List) ->
    {List, List}.

%%---------------------------------------------------------------------------
%% @doc
%%  Returns whether or not the node list is empty.
%%
%% @end
%%---------------------------------------------------------------------------
-spec is_empty(node_list()) -> boolean().
is_empty({_Cursor, List}) ->
    List == [].

%%---------------------------------------------------------------------------
%% @doc
%%  Returns the current element in the node list. Returns undefined if the node
%%  list is empty.
%%
%% @end
%%---------------------------------------------------------------------------
-spec focus(node_list()) -> any() | undefined.
focus({_Cursor, []}) ->
    undefined;
focus({Cursor, _List}) ->
    lists:nth(1, Cursor).

%%---------------------------------------------------------------------------
%% @doc
%%  Advances the focus by one element and returns the resultant node list.
%%
%% @end
%%---------------------------------------------------------------------------
-spec step(node_list()) -> node_list().
step({[_Element | []], List}) ->
    {List, List};
step({[_Element | Tail], List}) ->
    {Tail, List}.

%%---------------------------------------------------------------------------
%% @doc
%%  Returns the number of nodes in the node list.
%%
%% @end
%%---------------------------------------------------------------------------
-spec size(node_list()) -> integer().
size({_Cursor, List}) ->
    erlang:length(List).

%%---------------------------------------------------------------------------
%% @doc
%%  Inserts an element into the node list in random order and returns the
%%  resultant node list.
%%
%% @end
%%---------------------------------------------------------------------------
-spec add(any(), node_list()) -> node_list().
add(Node, {_Cursor, List}) ->
    {_Cursor, [Node | List]}.

%%---------------------------------------------------------------------------
%% @doc
%%  Fast-forward the list until the focus is on Node. Returns {ok, NewList} or
%%  {error, Reason} if Node was not found.
%%
%% @end
%%---------------------------------------------------------------------------
-spec seek(any(), node_list()) -> {ok, node_list()} | {error, term()}.
seek(Node, {_Cursor, List}) ->
    case lists:dropwhile(fun(N) -> N /= Node end, List) of
        [] ->
            {error, not_found};
        L ->
            {ok, {L, List}}
    end.

%%---------------------------------------------------------------------------
%% @doc
%%  Removes the focus element and returns the resultant node list.
%%
%% @end
%%---------------------------------------------------------------------------
-spec drop(node_list()) -> node_list().
drop({[Head | Tail], List}) ->
    NewList = lists:delete(Head, List),
    case Tail of
        [] ->
            {NewList, NewList};
        _ ->
            {Tail, NewList}
    end.
