%%%-------------------------------------------------------------------------
%%% @author Ryan Burrows <rhburrows@gmail.com>
%%% @doc
%%%  DTask server for scheduling a distributed repeated tasks
%%% @end
%%%-------------------------------------------------------------------------
-module(dtask_timer).

-behavior(gen_server).

%% API
-export([start_link/0, schedule/4, cancel/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-type tref() :: reference().

-record(task, { id        :: tref(),
                module    :: module(),
                function  :: term(),
                arguments :: dtask:args() }).

%%%==========================================================================
%%% API
%%%==========================================================================

%%---------------------------------------------------------------------------
%% @doc
%%  Starts the DTask server
%% @end
%%---------------------------------------------------------------------------
start_link() ->
    gen_server:start_link({global, ?MODULE}, ?MODULE, [], []).

%%---------------------------------------------------------------------------
%% @doc
%%  Schedule a task to be executed repeatedly at intervals of Time. The task
%%  will call Function when it is executed and will be executed on one of the
%%  worker nodes registered with DTask
%%
%%  Returns {ok, TRef} or {error, Reason}
%% @end
%%---------------------------------------------------------------------------
-spec schedule(timeout(), module(), term(), dtask:args()) ->
                      {ok, tref()} | {error, term()}.
schedule(Time, Module, Function, Arguments) ->
    gen_server:call({global, ?MODULE},
                    {schedule, Time, Module, Function, Arguments}).

%%---------------------------------------------------------------------------
%% @doc
%%  Cancel a previously scheduled task. TRef is a unique task reference
%%  returned by the schedule function.
%%
%%  Returns {ok, cancel} or {error, Reason}
%% @end
%%---------------------------------------------------------------------------
-spec cancel(tref()) -> {ok, cancel} | {error, term()}.
cancel(TRef) ->
    gen_server:call({global, ?MODULE}, {cancel, TRef}).

%%%==========================================================================
%%% gen_server callbacks
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
handle_call({schedule, Time, Module, Function, Arguments}, _From, Tasks) ->
    Task = create_task(Time, Module, Function, Arguments),
    {reply, {ok, Task#task.id}, [Task | Tasks]};
handle_call({cancel, TRef}, _From, Tasks) ->
    case remove_task(TRef, Tasks) of
        {not_found, _Tasks} ->
            {reply, {error, badarg}, Tasks};
        {_Task, RemainingTasks} ->
            {reply, {ok, cancel}, RemainingTasks}
    end;
handle_call(stop, _From, Tasks) ->
    {stop, normal, stopped, Tasks}.

%%---------------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%---------------------------------------------------------------------------
handle_cast(stop, S) ->
    {stop, normal, S};
handle_cast(_Msg, S) ->
    {noreply, S}.

%%---------------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%---------------------------------------------------------------------------
handle_info(_Info, S) ->
    {noreply, S}.

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
code_change(_OldVsn, S, _Extra) ->
    {ok, S}.

%%%==========================================================================
%%% helper functions
%%%==========================================================================
create_task(Timeout, Module, Function, Arguments) ->
    {ok, TRef} = timer:apply_interval(Timeout,
                                      dtask,
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
