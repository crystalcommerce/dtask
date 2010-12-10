%%%--------------------------------------------------------------------------
%%% @author Ryan Burrows <rhburrows@gmail.com>
%%% @doc
%%%  DTask server for executing a function call distributed across a
%%%  a collection of registered nodes. DTask will help balance the execution
%%%  of task to help prevent overloading a single system.
%%% @end
%%%--------------------------------------------------------------------------
-module(dtask).
-export([start/0, stop/0, call/3, cast/3]).

%%---------------------------------------------------------------------------
%% @doc
%%  Start the DTask application
%% @end
%%---------------------------------------------------------------------------
-spec start() -> ok | {error, term()}.
start() ->
    application:start(dtask).

%%---------------------------------------------------------------------------
%% @doc
%%  Stop the DTask application
%% @end
%%---------------------------------------------------------------------------
-spec stop() -> ok | {error, term()}.
stop() ->
    application:stop(dtask).

%%---------------------------------------------------------------------------
%% @doc
%%  Evaluates apply(Module, Function, Args) on a remote node that is
%%  registered with DTask. Returns ok or {error, Reason}. If there are no
%%  nodes registered with DTask Reason is no_node.
%% @end
%%---------------------------------------------------------------------------
-spec call(module(), term(), dtask_srv:args()) -> ok | {error, term()}.
call(Module, Function, Args) ->
    dtask_srv:call(Module, Function, Args).

%%---------------------------------------------------------------------------
%% @doc
%%  Evaluates apply(Module, Function, Args) asynchronously on a remote node
%%  that is registered with DTask. Returns ok or {error, Reason}. If there are
%%  no nodes registered with DTask Reason is no_node.
%% @end
%%---------------------------------------------------------------------------
-spec cast(module(), term(), dtask_srv:args()) -> ok. 
cast(Module, Function, Args) ->
    dtask_srv:cast(Module, Function, Args).
