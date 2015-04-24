-module(ekv_store).

-author("Falcon.C <falcom520@gmail.com>").

-export([start_link/0,find/1,find/2,
         add/3,
         delete/2
        ]).

-export([ init/1,
          handle_call/3,
          handle_cast/2,
          handle_info/2,
          terminate/2,
          code_change/3
        ]).

-record(bigtable,{user_id,ekv_key,ekv_val}).
-record(state,{max=0}).

start_link() ->
    gen_server:start_link({local,?MODULE},?MODULE,[],[]).

find(User_id) when is_binary(User_id) ->
        case mnesia:dirty_read(bigtable,User_id) of
            [Bt] ->
                {bigtable,User_id,Ekv_key,Ekv_val} = Bt,
                [{k,iolist_to_binary(Ekv_key)},{v,iolist_to_binary(Ekv_val)}];
            [] ->
                []
        end.

find(User_id,Key) when is_binary(User_id) and is_binary(Key) ->
    F = fun() ->
        Match = #bigtable{user_id=User_id,ekv_key=Key,ekv_val='$1'},
            Guard = [],
            Result = ['$1'],
        mnesia:select(bigtable,[{Match,Guard,Result}])
    end,
    case mnesia:transaction(F) of
        {atomic,[Data]} ->
            [{k,Key},{v,Data}];
        _ ->
            []
    end.


add(User_id,Key,Val) when is_binary(User_id) and is_binary(Key) and is_binary(Val) ->
    Btable = #bigtable{user_id=User_id,ekv_key=Key,ekv_val=Val},
    mnesia:dirty_write(Btable),
    mnesia:dirty_read({bigtable,User_id}).

delete(User_id,Key) when is_binary(User_id) and is_binary(Key) ->
    F = fun() ->
        Match = #bigtable{user_id=User_id,ekv_key=Key,ekv_val='$1'},
            Guard = [],
            Result = ['$1'],
        mnesia:select(bigtable,[{Match,Guard,Result}])
    end,
    case mnesia:transaction(F) of
        {atomic,[Data]} ->
            F1 = fun() ->
                mnesia:delete_object(#bigtable{user_id=User_id,ekv_key=Key,ekv_val=Data})
            end,
            mnesia:transaction(F1);
        _ ->
            ok
    end.


init([]) ->
    case mnesia:system_info(extra_db_nodes) of
        [] -> mnesia:create_schema([node()]);
        _ -> ok
    end,
    ok = mnesia:start(),
    mnesia:create_table(bigtable,[
            {type,bag},
            {disc_copies,[node()]},
            {record_name,bigtable},
            {attributes,record_info(fields,bigtable)}
            ]),
    mnesia:wait_for_tables(mnesia:system_info(local_tables), infinity),
    {ok,#state{}}.

handle_call(_Req,_From,State) ->
    {reply,error,State}.

handle_cast(_Msg,State) ->
    {noreply,State}.


handle_info(_Info,State) ->
    {noreply,State}.


terminate(_Reason,_State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok,State}.






