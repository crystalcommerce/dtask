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
-export([start_link/0, call/3, cast/3, register/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-type args() :: list().

-export_type([args/0]).

%%%==========================================================================
%%% API
%%%==========================================================================
start_link() ->
    gen_server:start_link({global, ?MODULE},
                          ?MODULE,
                          dtask_node_list:new([]),
                          []).

%%---------------------------------------------------------------------------
%% @doc
%%  Evaluates apply(Module, Function, Args) on a remote node that is
%%  registered with DTask. Returns ok or {error, Reason}. If there are no
%%  nodes registered with DTask Reason is no_node.
%% @end
%%---------------------------------------------------------------------------
-spec call(module(), term(), args()) -> ok | {error, term()}.
call(Module, Function, Args) ->
    gen_server:call({global, ?MODULE}, {apply, Module, Function, Args}).

%%---------------------------------------------------------------------------
%% @doc
%%  Evaluates apply(Module, Function, Args) asynchronously on a remote node
%%  that is registered with DTask. Returns ok or {error, Reason}. If there are
%%  no nodes registered with DTask Reason is no_node.
%% @end
%%---------------------------------------------------------------------------
-spec cast(module(), term(), args()) -> ok. 
cast(Module, Function, Args) ->
    gen_server:cast({global, ?MODULE}, {apply, Module, Function, Args}).

-spec register(node()) -> ok.
register(Node) ->
    gen_server:call({global, ?MODULE}, {register, Node}).

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
    {stop, normal, stopped, Nodes};

handle_call({register, Node}, _From, Nodes) ->
    {reply, ok, dtask_node_list:add(Node, Nodes)};

handle_call({apply, Module, Function, Args}, _From, Nodes) ->
    {Response, NewNodes} = apply(Module, Function, Args, Nodes),
    {reply, Response, NewNodes}.

%%---------------------------------------------------------------------------
%% @private
%% @doc
%%  handle_cast gen_server callback
%% @end
%%---------------------------------------------------------------------------
handle_cast({apply, Module, Function, Args}, Nodes) ->
    {_, NewNodes} = apply(Module, Function, Args, Nodes),
    {noreply, NewNodes}.

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

%%---------------------------------------------------------------------------
%% @private
%% @doc
%%  Evaluates apply(Module, Function, Args) on a remote node that is
%%  registerd with DTask.
%% @end
%%---------------------------------------------------------------------------
apply(Module, Function, Args, Nodes) ->
    case dtask_node_list:is_empty(Nodes) of 
        true -> 
            {{error, no_node}, Nodes};
        _ -> 
            Result = rpc:call(dtask_node_list:focus(Nodes), Module, Function, Args),
            {Result, dtask_node_list:step(Nodes)}
    end.
