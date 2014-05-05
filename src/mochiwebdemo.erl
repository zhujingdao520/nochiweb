%% @author Mochi Media <dev@mochimedia.com>
%% @copyright 2010 Mochi Media <dev@mochimedia.com>

%% @doc mochiwebdemo.

-module(mochiwebdemo).
-author("Mochi Media <dev@mochimedia.com>").
-export([start/0, stop/0]).

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.


%% @spec start() -> ok
%% @doc Start the mochiwebdemo server.
start() ->
    mochiwebdemo_deps:ensure(),
    ensure_started(crypto),
    application:start(mochiwebdemo).


%% @spec stop() -> ok
%% @doc Stop the mochiwebdemo server.
stop() ->
    application:stop(mochiwebdemo).
