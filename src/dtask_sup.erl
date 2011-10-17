-module(dtask_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(WORKER(I, Args), {I, {I, start_link, [Args]}, permanent, 5000, worker, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    case application:get_env(dtask, nodes) of
        undefined ->
            error_logger:error_msg("No nodes found for dtask in config", []),
            {error, no_config};
        {ok, Nodes} ->
            {ok, { {one_for_one, 5, 10},
                   [
                    ?WORKER(dtask_srv, Nodes),
                    ?WORKER(dtask_timer, Nodes)
                   ]}}
    end.
