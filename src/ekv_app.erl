%% @author Falcon.C  <falcom520@gmail.com>
%% @copyright ekv Falcon.C  <falcom520@gmail.com>

%% @doc Callbacks for the ekv application.

-module(ekv_app).
-author("Falcon.C  <falcom520@gmail.com>").

-behaviour(application).
-export([start/2,stop/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for ekv.
start(_Type, _StartArgs) ->
    ekv_deps:ensure(),
    ekv_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for ekv.
stop(_State) ->
    ok.
