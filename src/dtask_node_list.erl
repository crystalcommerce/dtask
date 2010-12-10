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
         seek/2, drop/1, remove/2, difference/2, concat/2]).

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
focus({[Head | _Rest], _List}) ->
    Head.

%%---------------------------------------------------------------------------
%% @doc
%%  Advances the focus by one element and returns the resultant node list.
%%
%% @end
%%---------------------------------------------------------------------------
-spec step(node_list()) -> node_list().
step({[], []}) ->
    {[], []};
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
add(Node, {[], []}) ->
    {[Node], [Node]};
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

%%---------------------------------------------------------------------------
%% @doc
%%  Remove Node from the nodelist. The focus will stay the same unless the
%%  node list is currently focused on the element being removed. In this case
%%  the resultant node list will have the focus on the next element.
%% @end
%%---------------------------------------------------------------------------
-spec remove(any(), node_list()) -> node_list().
remove(Node, Nodes={[Focus | _Tail], _List}) when Focus == Node ->
    dtask_node_list:drop(Nodes);

remove(Node, List) ->
    Focus = dtask_node_list:focus(List),
    case dtask_node_list:seek(Node, List) of
        {ok, Nodes} ->
            {ok, Nodes1} = dtask_node_list:seek(Focus, dtask_node_list:drop(Nodes)),
            Nodes1;
        Error ->
            Error
    end.

%%---------------------------------------------------------------------------
%% @doc
%%  Return a new nodelist containing the set difference between two node list
%%  arguments
%% @end
%%---------------------------------------------------------------------------
-spec difference(node_list(), node_list()) -> node_list().
difference(List1, {_, List2Nodes}) ->
    lists:foldl(fun(Node, Nodes) ->
                        case remove(Node, Nodes) of
                            {error, _} ->
                                Nodes;
                            Nodes1 ->
                                Nodes1
                        end
                end, List1, List2Nodes).

%%---------------------------------------------------------------------------
%% @doc
%%  Return a new nodelist containing the elements of List1 and the elements of
%%  List2. The focus will remain where it was on List1. If there is an entry
%%  that is the same in both lists the returned list will contain a duplicate
%% @end
%%---------------------------------------------------------------------------
-spec concat(node_list(), node_list()) -> node_list().
concat({[], []}, List2) ->
    List2;

concat({[Focus | _T], List1}, {_, List2}) ->
    NodeList = new(List1 ++ List2),
    {ok, New} = seek(Focus, NodeList),
    New.
