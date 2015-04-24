%% @author Falcon.C  <falcom520@gmail.com>
%% @copyright 2015 Falcon.C  <falcom520@gmail.com>

%% @doc Ensure that the relatively-installed dependencies are on the code
%%      loading path, and locate resources relative
%%      to this application's path.

-module(ekv_res).
-author("Falcon.C  <falcom520@gmail.com>").


-export([init/1,
        get_resource/1,
        put_resource/1,
        delete_resource/1]).

-include("ekv.hrl").

init([]) ->
    ok.

get_resource(Req) ->
    Path = Req:get(path),
    User_id = list_to_binary(Req:get_header_value(user_id)),
    case string:strip(Path,right,$/) of
        "/api/read" ->
            Qs = Req:parse_qs(),
            Key = iolist_to_binary(proplists:get_value("k",Qs)),
            Returndata = ekv_store:find(User_id,Key),
            Data = mochijson2:encode({struct,Returndata}),
            Req:respond({200,?JSON_HEADER,iolist_to_binary(Data)});
        _ ->
            Req:respond({404,?JSON_HEADER,mochijson2:encode({struct,?JSON_404})})
    end.

put_resource(Req) ->
    Path = Req:get(path),
    User_id = list_to_binary(Req:get_header_value(user_id)),
    case string:strip(Path,right,$/) of
        "/api/add" ->
            Qs = mochiweb_util:parse_qs(Req:recv_body()),
            case {proplists:get_value("k",Qs),proplists:get_value("v",Qs)} of
                {undefined,undefined} ->
                    Data = mochijson2:encode({struct,[{ret,10002},{msg,<<"k and v is not null">>}]});
                {undefined,_E} ->
                    Data = mochijson2:encode({struct,[{ret,10001},{msg,<<"k is not null">>}]});
                {_E,undefined} ->
                    Data = mochijson2:encode({struct,[{ret,10002},{msg,<<"v is not null">>}]});
                {Key,Val} ->
                    ekv_store:add(User_id,iolist_to_binary(Key),iolist_to_binary(Val)),
                    Data = mochijson2:encode({struct,[{ret,200},{msg,<<"ok">>}]})
            end,
            Req:respond({200,?JSON_HEADER,iolist_to_binary(Data)});
        _ ->
            Req:respond({404, ?JSON_HEADER,mochijson2:encode({struct,?JSON_404})})
    end.

delete_resource(Req) ->
    Path = Req:get(path),
    User_id = list_to_binary(Req:get_header_value(user_id)),
    case string:strip(Path,right,$/) of
        "/api/delete" ->
            Qs = Req:parse_qs(),
            case proplists:get_value("k",Qs) of
                undefined ->
                    Data = mochijson2:encode({struct,[{ret,10001},{msg,<<"k is not null">>}]});
                Key ->
                    ekv_store:delete(User_id,list_to_binary(Key)),
                    Data = mochijson2:encode({struct,[{ret,200},{msg,<<"ok">>}]})
            end,
            Req:respond({200,?JSON_HEADER,iolist_to_binary(Data)});
        _ ->
            Req:respond({404, ?JSON_HEADER, ?JSON_404})
    end.




