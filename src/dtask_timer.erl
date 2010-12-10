%%%-------------------------------------------------------------------------
%%% @author Ryan Burrows <rhburrows@gmail.com>
%%% @doc
%%%  DTask server for scheduling a distributed repeated tasks
%%% @end
%%%-------------------------------------------------------------------------
-module(dtask_timer).

-behavior(gen_leader).

%% API
-export([start_link/1, apply_interval/4, cancel/1]).

%% gen_leader callbacks
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

-record(task, { id        :: timer:tref(),
                module    :: module(),
                function  :: term(),
                arguments :: dtask_srv:args(),
                timeout   :: timeout() }).

%%%==========================================================================
%%% API
%%%==========================================================================

%%---------------------------------------------------------------------------
%% @doc
%%  Starts the DTask server
%% @end
%%---------------------------------------------------------------------------
start_link(Nodes) ->
    gen_leader:start_link(?MODULE,
                          Nodes,
                          [],
                          ?MODULE,
                          [],
                          []).

%%---------------------------------------------------------------------------
%% @doc
%%  Schedule a task to be executed repeatedly at intervals of Time. The task
%%  will call Function when it is executed and will be executed on one of the
%%  worker nodes registered with DTask
%%
%%  Returns {ok, TRef} or {error, Reason}
%% @end
%%---------------------------------------------------------------------------
-spec apply_interval(timeout(), module(), term(), dtask_srv:args()) ->
                      {ok, timer:tref()} | {error, term()}.
apply_interval(Time, Module, Function, Arguments) ->
    gen_leader:leader_call(?MODULE,
                    {schedule, Time, Module, Function, Arguments}).

%%---------------------------------------------------------------------------
%% @doc
%%  Cancel a previously scheduled task. TRef is a unique task reference
%%  returned by the schedule function.
%%
%%  Returns {ok, cancel} or {error, Reason}
%% @end
%%---------------------------------------------------------------------------
-spec cancel(timer:tref()) -> {ok, cancel} | {error, term()}.
cancel(TRef) ->
    gen_leader:leader_call(?MODULE, {cancel, TRef}).

%%%==========================================================================
%%% gen_leader callbacks
%%%==========================================================================

%%---------------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%---------------------------------------------------------------------------
init([]) ->
    {ok, []}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%  Handling call messages
%% @end
%%--------------------------------------------------------------------
-spec handle_call(term(), pid(), list(#task{}), gen_leader:election()) ->
                         {reply, term(), list(#task{})} |
                         {noreply, list(#task{})} |
                         {stop, term(), term(), list(#task{})} |
                         {stop, term(), list(#task{})}.
handle_call(stop, _From, Tasks, _Election) ->
    {stop, normal, stopped, Tasks}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%  Handling cast messages
%% @end
%%--------------------------------------------------------------------
-spec handle_cast(term(), list(#task{}), gen_leader:election()) ->
                         {noreply, list(#task{})} |
                         {stop, term(), list(#task{})}.
handle_cast(stop, S, _Election) ->
    {stop, normal, S};
handle_cast(_Msg, S, _Election) ->
    {noreply, S}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%  Handling all non call/cast messages
%% @end
%%--------------------------------------------------------------------
-spec handle_info(term(), list(#task{})) -> {noreply, list(#task{})}.
handle_info(_Info, S) ->
    {noreply, S}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%  Handling call messages. Called in the leader.
%% @end
%%--------------------------------------------------------------------
-spec handle_leader_call(term(), pid(), list(#task{}), gen_leader:election()) ->
                                {reply, ok, term(), list(#task{})} |
                                {reply, term(), list(#task{})} |
                                {noreply, list(#task{})} |
                                {stop, term(), term(), list(#task{})} |
                                {stop, term(), list(#task{})}.
handle_leader_call({schedule, Time, Module, Function, Arguments}, 
                   _From, Tasks, _Election) ->
    Task = create_task(Time, Module, Function, Arguments),
    {reply, {ok, Task#task.id}, [Task | Tasks]};

handle_leader_call({cancel, TRef}, _From, Tasks, _Election) ->
    case remove_task(TRef, Tasks) of
        {not_found, _Tasks} ->
            {reply, {error, badarg}, Tasks};
        {_Task, RemainingTasks} ->
            {reply, {ok, cancel}, RemainingTasks}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%  Handling cast messages. Called in the leader.
%% @end
%%--------------------------------------------------------------------
-spec handle_leader_cast(term(), list(#task{}), gen_leader:election()) ->
                                {ok, term(), list(#task{})} |
                                {noreply, list(#task{})} |
                                {stop, term(), list(#task{})}.
handle_leader_cast(_Request, State, _Election) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%  Handling messages from leader.
%% @end
%%--------------------------------------------------------------------
-spec from_leader(term(), list(#task{}), gen_leader:election()) ->
                         {ok, list(#task{})} |
                         {noreply, list(#task{})} |
                         {stop, term(), list(#task{})}.
from_leader(_Synch, State, _Election) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%  Handling nodes going down. Called in the leader only.
%% @end
%%--------------------------------------------------------------------
-spec handle_DOWN(node(), list(#task{}), gen_leader:election()) ->
                         {ok, list(#task{})} |
                         {ok, term(), list(#task{})}.
handle_DOWN(_Node, State, _Election) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Called only in the leader process when it is elected. The Synch
%% term will be broadcasted to all the nodes in the cluster.
%% @end
%%--------------------------------------------------------------------
-spec elected(list(#task{}), gen_leader:election(), term()) ->
                                       {ok, term(), list(#task{})}.
elected(State, _Election, undefined) ->
    Synch = [],
    NewState = lists:map(fun(Task) -> 
                Ref = timer:apply_interval(Task#task.timeout, 
                                            Task#task.module,
                                            Task#task.function,
                                            Task#task.arguments),
                Task#task{id = Ref}
              end, State),
    {ok, Synch, NewState};

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Called only in the leader process when a new candidate joins the
%% cluster. The Synch term will be sent to Node.
%% @end
%%--------------------------------------------------------------------
elected(State, _Election, _Node) ->
    {reply, [], State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Called in all members of the cluster except the leader. Synch is a
%% term returned by the leader in the elected/3 callback.
%% @end
%%--------------------------------------------------------------------
-spec surrendered(list(#task{}), term(), gen_leader:election()) ->
                               {ok, list(#task{})}.
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
-spec terminate(term(), list(#task{})) -> 
                       ok.
terminate(_Reason, _S) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%  Convert process state when code is changed
%% @end
%%--------------------------------------------------------------------
-spec code_change(string(), list(#task{}), gen_leader:election(), any()) ->
                         {ok, list(#task{})} |
                         {ok, list(#task{}), gen_leader:election()}.
code_change(_OldVsn, S, _Election, _Extra) ->
    {ok, S}.

%%%==========================================================================
%%% helper functions
%%%==========================================================================
-spec create_task(timeout(), module(), term(), dtask_srv:args()) ->
                         #task{}.
create_task(Timeout, Module, Function, Arguments) ->
    {ok, TRef} = timer:apply_interval(Timeout,
                                      dtask_srv,
                                      cast,
                                      [Module, Function, Arguments]),
    #task{ id        = TRef,
           module    = Module,
           function  = Function,
           timeout   = Timeout,
           arguments = Arguments }.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%  Removes a task from the state by reference.
%% @end
%%--------------------------------------------------------------------
-spec remove_task(timer:tref(), list(#task{})) ->
                               {#task{}, list(#task{})} |
                               {#task{}, list(#task{})}.
remove_task(TRef, Tasks) ->
    lists:foldr(fun(Task, {T, Remaining}) -> case Task#task.id of
                            TRef ->
                                {Task, Remaining};
                            _ ->
                                {T, [Task | Remaining]}
                        end
                end, {not_found, []}, Tasks).
