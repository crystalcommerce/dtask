%%%--------------------------------------------------------------------------
%%% @author Ryan Burrows <rhburrows@gmail.com>
%%% @doc
%%%  DTask server for executing a function call distributed across a
%%%  a collection of registered nodes. DTask will help balance the execution
%%%  of task to help prevent overloading a single system.
%%% @end
%%%--------------------------------------------------------------------------
-module(dtask).

-behavior(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%%%==========================================================================
%%% API
%%%==========================================================================
start_link() ->
    gen_server:start_link({global, ?MODULE}, ?MODULE, [], []).

%%%==========================================================================
%%% gen_server callbacks
%%%==========================================================================

%%---------------------------------------------------------------------------
%% @private
%% @doc
%%  Initialize the DTask server. Nodes is a list of nodes to distribute the
%%  tasks between
%% @end
%%---------------------------------------------------------------------------
init(Nodes) ->
    {ok, Nodes}.

%%---------------------------------------------------------------------------
%% @private
%% @doc
%%  handle_call gen_server callback
%% @end
%%---------------------------------------------------------------------------
handle_call(stop, _From, Nodes) ->
    {stop, normal, stopped, Nodes}.

%%---------------------------------------------------------------------------
%% @private
%% @doc
%%  handle_cast gen_server callback
%% @end
%%---------------------------------------------------------------------------
handle_cast(_Msg, Nodes) ->
    {noreply, Nodes}.

%%---------------------------------------------------------------------------
%% @private
%% @doc
%%  handle_info gen_server callback
%% @end
%%---------------------------------------------------------------------------
handle_info(_Info, Nodes) ->
    {noreply, Nodes}.

%%---------------------------------------------------------------------------
%% @private
%% @doc
%%  terminate gen_server callback
%% @end
%%---------------------------------------------------------------------------
terminate(_Reason, _Nodes) ->
    ok.

%%---------------------------------------------------------------------------
%% @private
%% @doc
%%  code_change gen_server callback
%% @end
%%---------------------------------------------------------------------------
code_change(_OldVsn, Nodes, _Extra) ->
    {ok, Nodes}.
