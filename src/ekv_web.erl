%% @author Falcon.C  <falcom520@gmail.com>
%% @copyright 2015 Falcon.C  <falcom520@gmail.com>

%% @doc Web server for ekv.

-module(ekv_web).
-author("Falcon.C  <falcom520@gmail.com>").

-export([start/1, stop/0, loop/2]).

-include("ekv.hrl").

%% External API

start(Options) ->
    {DocRoot, Options1} = get_option(docroot, Options),
    Loop = fun (Req) ->
                   ?MODULE:loop(Req, DocRoot)
           end,
    ekv_store:start_link(),
    mochiweb_http:start([{name, ?MODULE}, {loop, Loop} | Options1]).

stop() ->
    mochiweb_http:stop(?MODULE).

loop(Req, DocRoot) ->
    try
        case ekv_auth:check(Req) of
            true ->
                case Req:get(method) of
                    Method when Method =:= 'GET'; Method =:= 'HEAD' ->
                        ekv_res:get_resource(Req);
                    'PUT' ->
                        ekv_res:put_resource(Req);
                    'DELETE' ->
                        ekv_res:delete_resource(Req);
                    _ ->
                        Req:respond({500,?JSON_HEADER, []})
                end;
            false ->
                Req:respond({403,?JSON_HEADER,"Authorization errors\n"})
        end
    catch
        Type:What ->
            Path = Req:get(path),
            Report = ["web request failed",
                      {path, Path},
                      {type, Type}, {what, What},
                      {trace, erlang:get_stacktrace()}],
            error_logger:error_report(Report),
            Req:respond({500,?JSON_HEADER,
                         ""})
    end.

%% Internal API

get_option(Option, Options) ->
    {proplists:get_value(Option, Options), proplists:delete(Option, Options)}.

%%
%% Tests
%%
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

you_should_write_a_test() ->
    ?assertEqual(
       "No, but I will!",
       "Have you written any tests?"),
    ok.

-endif.
