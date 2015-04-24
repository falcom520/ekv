%% @author Falcon.C  <falcom520@gmail.com>
%% @copyright 2015 Falcon.C  <falcom520@gmail.com>

%% @doc HTTP API Request authorized
%%      
%%      

-module(ekv_auth).
-author("Falcon.C  <falcom520@gmail.com>").

-export([init/1,
        check/1,
        add/2,
        delete/1]).


-record(ekv_user,{
	    username::binary(),
        password::binary()
	}).

-define(USER_TAB, ekv_user).

init(_Opts) ->
    mnesia:create_table(?USER_TAB, [
            {ram_copies, [node()]}, 
            {attributes, record_info(fields, ekv_user)}]),
    mnesia:add_table_copy(?USER_TAB, node(), ram_copies),
    ok.

check(Req) ->
    Username = list_to_binary(Req:get_header_value(user_id)),
    Password = list_to_binary(Req:get_header_value(secure_id)),
    case {validate(user_id,Username),validate(password,Password)} of
        {true,true} ->
            %%check(Username,Password);
            true;
        {_,false} ->
            false;
        {false,_} ->
            false
    end.
    %%check(Username,Password).
    %%true.

check(undefined, _) -> false;

check(_, undefined) -> false;

check(Username, Password) when is_binary(Username), is_binary(Password) ->
    Password = crypto:hash(md5, Password),case mnesia:dirty_read(?USER_TAB, Username) of
        [#ekv_user{password=Password}] -> true;
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
