%% @author Falcon.C  <falcom520@gmail.com>
%% @copyright 2015 Falcon.C  <falcom520@gmail.com>

%% @doc HTTP API Request authorized
%%      
%%      

-module(ekv_auth).
-author("Falcon.C  <falcom520@gmail.com>").

-export([init/0,
        check/1,
        add/2,
        delete/1]).


-record(ekv_user,{
	    username,
        password
	}).

-define(USER_TAB, ekv_user).

init() ->
    mnesia:create_table(?USER_TAB, [
            {type,set},
            {disc_copies, [node()]},
            {record_name,ekv_user},
            {attributes, record_info(fields, ekv_user)}]),
    ok.

check(Req) ->
    Username = list_to_binary(Req:get_header_value(user_id)),
    Password = list_to_binary(Req:get_header_value(secure_id)),
    case {validate(user_id,Username),validate(password,Password)} of
        {_,false} ->
            false;
        {false,_} ->
            false;
        {true,true} ->
            check(Username,Password)
    end.

check(undefined, _) -> false;

check(_, undefined) -> false;

check(Username, Password) ->
    Passwd = crypto:hash(md5, Password),
    case mnesia:dirty_read(?USER_TAB, Username) of
        [#ekv_user{password=Passwd}] -> true;
        _ -> false
    end.

add(Username, Password) when is_binary(Username) and is_binary(Password) ->
    mnesia:dirty_write(
        #ekv_user{
            username=Username, 
            password=crypto:hash(md5, Password)
        }
    ).

delete(Username) when is_binary(Username) ->
    mnesia:dirty_delete(?USER_TAB, Username).



validate(user_id,User_id) ->
    is_binary(User_id);
validate(password,Password) ->
    is_binary(Password).
