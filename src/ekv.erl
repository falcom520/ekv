%% @author Falcon.C  <falcom520@gmail.com>
%% @copyright 2015 Falcon.C  <falcom520@gmail.com>

%% @doc ekv.

-module(ekv).
-author("Falcon.C  <falcom520@gmail.com>").
-export([start/0, stop/0]).

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.


%% @spec start() -> ok
%% @doc Start the ekv server.
start() ->
    ekv_deps:ensure(),
    ensure_started(crypto),
    application:start(ekv).


%% @spec stop() -> ok
%% @doc Stop the ekv server.
stop() ->
    application:stop(ekv).
