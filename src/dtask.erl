%%%--------------------------------------------------------------------------
%%% @author Ryan Burrows <rhburrows@gmail.com>
%%% @doc
%%%  DTask server for executing a function call distributed across a
%%%  a collection of registered nodes. DTask will help balance the execution
%%%  of task to help prevent overloading a single system.
%%% @end
%%%--------------------------------------------------------------------------
-module(dtask).
-export([start/0, stop/0]).

%% @spec start() -> ok
%% @doc Start the mnesia_graphs server.
start() ->
    application:start(dtask).

%% @spec stop() -> ok
%% @doc Stop the mnesia_graphs server.
stop() ->
    application:stop(dtask).
