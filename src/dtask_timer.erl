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
                arguments :: dtask_srv:args() }).

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
    gen_leader:call(?MODULE,
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
    gen_leader:call(?MODULE, {cancel, TRef}).

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

%%---------------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%---------------------------------------------------------------------------
handle_call({schedule, Time, Module, Function, Arguments}, _From, Tasks, _Election) ->
    Task = create_task(Time, Module, Function, Arguments),
    {reply, {ok, Task#task.id}, [Task | Tasks]};
handle_call({cancel, TRef}, _From, Tasks, _Election) ->
    case remove_task(TRef, Tasks) of
        {not_found, _Tasks} ->
            {reply, {error, badarg}, Tasks};
        {_Task, RemainingTasks} ->
            {reply, {ok, cancel}, RemainingTasks}
    end;
handle_call(stop, _From, Tasks, _Election) ->
    {stop, normal, stopped, Tasks}.

%%---------------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%---------------------------------------------------------------------------
handle_cast(stop, S, _Election) ->
    {stop, normal, S};
handle_cast(_Msg, S, _Election) ->
    {noreply, S}.

%%---------------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%---------------------------------------------------------------------------
handle_info(_Info, S) ->
    {noreply, S}.

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
%% @end
%%---------------------------------------------------------------------------
terminate(_Reason, _S) ->
    ok.

%%---------------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%---------------------------------------------------------------------------
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
           arguments = Arguments }.

remove_task(TRef, Tasks) ->
    lists:foldr(fun(Task, {T, Remaining}) ->
                        case Task#task.id of
                            TRef ->
                                {Task, Remaining};
                            _ ->
                                {T, [Task | Remaining]}
                        end
                end, {not_found, []}, Tasks).
