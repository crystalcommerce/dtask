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

%%---------------------------------------------------------------------------
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
-spec init(list()) -> {ok, dtask_node_list:node_list()} |
                      ignore |
                      {stop, term()}.
init(Nodes) ->
    {ok, Nodes}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%  Handling call messages
%% @end
%%--------------------------------------------------------------------
-spec handle_call(term(), pid(), dtask_node_list:node_list(), gen_leader:election()) ->
                         {reply, term(), dtask_node_list:node_list()} |
                         {noreply, dtask_node_list:node_list()} |
                         {stop, term(), term(), dtask_node_list:node_list()} |
                         {stop, term(), dtask_node_list:node_list()}.
handle_call(stop, _From, Nodes, _Election) ->
    {stop, normal, stopped, Nodes};

handle_call({register, Node}, _From, Nodes, _Election) ->
    {reply, ok, dtask_node_list:add(Node, Nodes)};

handle_call({apply, Module, Function, Args}, _From, Nodes, _Election) ->
    {Response, NewNodes} = dcall(Module, Function, Args, Nodes),
    {reply, Response, NewNodes}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%  Handling cast messages
%% @end
%%--------------------------------------------------------------------
-spec handle_cast(term(), dtask_node_list:node_list(), gen_leader:election()) ->
                         {noreply, dtask_node_list:node_list()} |
                         {stop, term(), dtask_node_list:node_list()}.
handle_cast({apply, Module, Function, Args}, Nodes, _Election) ->
    NewNodes = dcast(Module, Function, Args, Nodes),
    {noreply, NewNodes}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%  Handling all non call/cast messages
%% @end
%%--------------------------------------------------------------------
-spec handle_info(term(), dtask_node_list:node_list()) ->
                         {noreply, dtask_node_list:node_list()} |
                         {stop, term(), dtask_node_list:node_list()}.
handle_info(_Info, Nodes) ->
    {noreply, Nodes}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%  Handling call messages. Called in the leader.
%% @end
%%--------------------------------------------------------------------
-spec handle_leader_call(term(), pid(), dtask_node_list:node_list(), gen_leader:election()) ->
                                {reply, term(), term(), dtask_node_list:node_list()} |
                                {reply, term(), dtask_node_list:node_list()} |
                                {noreply, dtask_node_list:node_list()} |
                                {stop, term(), term(), dtask_node_list:node_list()} |
                                {stop, term(), dtask_node_list:node_list()}.
handle_leader_call(stop, _From, Nodes, _Election) ->
    {stop, normal, stopped, Nodes};

handle_leader_call({apply, Module, Function, Args}, _From, Nodes, _Election) ->
    {Response, NewNodes} = dcall(Module, Function, Args, Nodes),
    {reply, Response, NewNodes}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%  Handling cast messages. Called in the leader.
%% @end
%%--------------------------------------------------------------------
-spec handle_leader_cast(term(), dtask_node_list:node_list(), gen_leader:election()) ->
                                {ok, term(), dtask_node_list:node_list()} |
                                {noreply, dtask_node_list:node_list()} |
                                {stop, term(), dtask_node_list:node_list()}.
handle_leader_cast({apply, Module, Function, Args}, Nodes, _Election) ->
    NewNodes = dcast(Module, Function, Args, Nodes),
    {noreply, NewNodes}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling messages from leader.
%%
%% @spec from_leader(Request, State, Election) ->
%%                                    {ok, State} |
%%                                    {noreply, State} |
%%                                    {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
from_leader(_Synch, State, _Election) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling nodes going down. Called in the leader only.
%%
%% @spec handle_DOWN(Node, State, Election) ->
%%                                  {ok, State} |
%%                                  {ok, Broadcast, State} |
%% @end
%%--------------------------------------------------------------------
handle_DOWN(_Node, State, _Election) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Called only in the leader process when it is elected. The Synch
%% term will be broadcasted to all the nodes in the cluster.
%%
%% @spec elected(Nodes, Election, undefined) -> {ok, Synch, Nodes}
%% @end
%%--------------------------------------------------------------------
elected(Nodes, _Election, undefined) ->
    Synch = [],
    {ok, Synch, Nodes};

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Called only in the leader process when a new candidate joins the
%% cluster. The Synch term will be sent to Node.
%%
%% @spec elected(Nodes, Election, Node) -> {ok, Synch, Nodes}
%% @end
%%--------------------------------------------------------------------
elected(Nodes, _Election, Node) ->
    {reply, [], dtask_node_list:add(Node, Nodes)}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Called in all members of the cluster except the leader. Synch is a
%% term returned by the leader in the elected/3 callback.
%%
%% @spec surrendered(State, Synch, Election) -> {ok, State}
%% @end
%%--------------------------------------------------------------------
surrendered(State, _Synch, _Election) ->
    {ok, State}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%%  This function is called by a gen_leader when it is about to
%%  terminate. It should be the opposite of Module:init/1 and do any
%%  necessary cleaning up. When it returns, the gen_leader terminates
%%  with Reason. The return value is ignored.
%% @end
%%--------------------------------------------------------------------
-spec terminate(term(), dtask_node_list:node_list()) ->
                       ok.
terminate(_Reason, _Nodes) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%  Convert process state when code is changed
%% @end
%%--------------------------------------------------------------------
-spec code_change(string(), dtask_node_list:node_list(), gen_leader:election(), any()) ->
                         {ok, dtask_node_list:node_list()} |
                         {ok, dtask_node_list:node_list(), gen_leader:election()}.
code_change(_OldVsn, Nodes, _Election, _Extra) ->
    {ok, Nodes}.

%%---------------------------------------------------------------------------
%% @private
%% @doc
%%  Evaluates apply(Module, Function, Args) on a remote node that is
%%  registered with DTask.
%% @end
%%---------------------------------------------------------------------------
-spec dcall(module(), atom(), args(), dtask_node_list:node_list()) ->
                    {any(), dtask_node_list:node_list()} |
                    {{error, term()}, dtask_node_list:node_list()}.
dcall(Module, Function, Args, Nodes) ->
    case dtask_node_list:is_empty(Nodes) of
        true -> 
            {{error, no_node}, Nodes};
        _ -> 
            Result = rpc:call(dtask_node_list:focus(Nodes), Module, Function, Args),
            {Result, dtask_node_list:step(Nodes)}
    end.

%%---------------------------------------------------------------------------
%% @private
%% @doc
%%  Evaluates apply(Module, Function, Args) on a remote node that is
%%  registered with DTask asynchronously.
%% @end
%%---------------------------------------------------------------------------
-spec dcast(module(), atom(), args(), dtask_node_list:node_list()) ->
                   {any(), dtask_node_list:node_list()} |
                   {{error, term()}, dtask_node_list:node_list()}.
dcast(Module, Function, Args, Nodes) ->
    case dtask_node_list:is_empty(Nodes) of
        true ->
            {{error, no_node}, Nodes};
        _ ->
            rpc:cast(dtask_node_list:focus(Nodes), Module, Function, Args),
            dtask_node_list:step(Nodes)
    end.
