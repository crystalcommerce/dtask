%%%--------------------------------------------------------------------------
%%% @author Ryan Burrows <rhburrows@gmail.com>
%%% @doc
%%%  DTask server for executing a function call distributed across a
%%%  a collection of registered nodes. DTask will help balance the execution
%%%  of task to help prevent overloading a single system.
%%% @end
%%%--------------------------------------------------------------------------
-module(dtask_srv).

-behavior(gen_leader).

%% API
-export([start_link/1, call/3, cast/3, register/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/4,
         handle_cast/3,
         handle_info/2,
         handle_leader_call/4,
         handle_leader_cast/3,
         handle_DOWN/3,
         elected/3,
         surrendered/3,
         from_leader/3,
         terminate/2,
         code_change/4]).

-type args() :: list().

-export_type([args/0]).

%%%==========================================================================
%%% API
%%%==========================================================================
start_link(Nodes) ->
    gen_leader:start_link(?MODULE,
                          Nodes,
                          [],
                          ?MODULE,
                          dtask_node_list:new([]),
                          []).

%% @doc
%%  Evaluates apply(Module, Function, Args) on a remote node that is
%%  registered with DTask. Returns ok or {error, Reason}. If there are no
%%  nodes registered with DTask Reason is no_node.
%% @end
%%---------------------------------------------------------------------------
-spec call(module(), term(), args()) -> ok | {error, term()}.
call(Module, Function, Args) ->
    gen_leader:call(?MODULE, {apply, Module, Function, Args}).

%%---------------------------------------------------------------------------
%% @doc
%%  Evaluates apply(Module, Function, Args) asynchronously on a remote node
%%  that is registered with DTask. Returns ok or {error, Reason}. If there are
%%  no nodes registered with DTask Reason is no_node.
%% @end
%%---------------------------------------------------------------------------
-spec cast(module(), term(), args()) -> ok. 
cast(Module, Function, Args) ->
    gen_leader:cast(?MODULE, {apply, Module, Function, Args}).

-spec register(node()) -> ok.
register(Node) ->
    gen_leader:call(?MODULE, {register, Node}).

%%%==========================================================================
%%% gen_leader callbacks
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
%%  handle_call gen_leader callback
%% @end
%%---------------------------------------------------------------------------
handle_call(stop, _From, Nodes, _Election) ->
    {stop, normal, stopped, Nodes};

handle_call({register, Node}, _From, Nodes, _Election) ->
    {reply, ok, dtask_node_list:add(Node, Nodes)};

handle_call({apply, Module, Function, Args}, _From, Nodes, _Election) ->
    {Response, NewNodes} = apply(Module, Function, Args, Nodes),
    {reply, Response, NewNodes}.

%%---------------------------------------------------------------------------
%% @private
%% @doc
%%  handle_cast gen_leader callback
%% @end
%%---------------------------------------------------------------------------
handle_cast({apply, Module, Function, Args}, Nodes, _Election) ->
    {_, NewNodes} = apply(Module, Function, Args, Nodes),
    {noreply, NewNodes}.

%%---------------------------------------------------------------------------
%% @private
%% @doc
%%  handle_info gen_leader callback
%% @end
%%---------------------------------------------------------------------------
handle_info(_Info, Nodes) ->
    {noreply, Nodes}.

handle_leader_call(_Request, _From, State, _Election) ->
    {reply, ok, State}.

handle_leader_cast(_Request, State, _Election) ->
    {noreply, State}.

from_leader(_Synch, State, _Election) ->
    {ok, State}.

handle_DOWN(_Node, State, _Election) ->
    {ok, State}.

elected(State, _Election, undefined) ->
    Synch = [],
    {ok, Synch, State};
elected(State, _Election, _Node) ->
    {reply, [], State}.

surrendered(State, _Synch, _Election) ->
    {ok, State}.


%%---------------------------------------------------------------------------
%% @private
%% @doc
%%  terminate gen_leader callback
%% @end
%%---------------------------------------------------------------------------
terminate(_Reason, _Nodes) ->
    ok.

%%---------------------------------------------------------------------------
%% @private
%% @doc
%%  code_change gen_leader callback
%% @end
%%---------------------------------------------------------------------------
code_change(_OldVsn, Nodes, _Election, _Extra) ->
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
