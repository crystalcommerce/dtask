-module(dtask_config).

-export([new/0, get/2]).

-type config() :: list({atom(), any()}).

%%---------------------------------------------------------------------------
%% @doc
%%   Returns a new config by loading values from the dtask.config file in the
%%   current directory. If the file does not exist return an empty config.
%% @end
%%---------------------------------------------------------------------------
-spec new() -> config().
new() ->
    {ok, Dir} = file:get_cwd(),
    ConfigName = case application:get_env(dtask, config) of
                     undefined ->
                         "dtask.config";
                     {ok, Path} ->
                         Path
                 end,
    File = filename:join([Dir, ConfigName]),
    case file:consult(File) of
        {ok, Terms} ->
            Terms;
        _Other ->
            []
    end.

%%---------------------------------------------------------------------------
%% @doc
%%   Lookup the key Option in Config. Return the value or undefined if Option
%%   isn't present.
%% @end
%%---------------------------------------------------------------------------
-spec get(atom(), config()) -> any() | undefined.
get(Option, Config) ->
    proplists:get_value(Option, Config, undefined).
