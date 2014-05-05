%% @author Mochi Media <dev@mochimedia.com>
%% @copyright mochiwebdemo Mochi Media <dev@mochimedia.com>

%% @doc Callbacks for the mochiwebdemo application.

-module(mochiwebdemo_app).
-author("Mochi Media <dev@mochimedia.com>").

-behaviour(application).
-export([start/2,stop/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for mochiwebdemo.
start(_Type, _StartArgs) ->
    mochiwebdemo_deps:ensure(),
    mochiwebdemo_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for mochiwebdemo.
stop(_State) ->
    ok.
