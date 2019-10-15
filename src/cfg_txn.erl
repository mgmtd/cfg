%%%-------------------------------------------------------------------
%%% @author Sean Hinde <sean@Seans-MacBook.local>
%%% @copyright (C) 2019, Sean Hinde
%%% @doc Configuration session transaction handler
%%%
%%% @end
%%% Created : 18 Sep 2019 by Sean Hinde <sean@Seans-MacBook.local>
%%%-------------------------------------------------------------------
-module(cfg_txn).

-include("cfg.hrl").

-record(cfg_txn,
        {
         txn_id,
         ops = [],
         ets_copy
         }).

-type txn() :: #cfg_txn{}.

-export_type([txn/0]).

-export([new/0, exit_txn/1, get/2, set/3, list_keys/2, commit/1]).

new() ->
    TxnId = erlang:unique_integer(),
    #cfg_txn{txn_id = TxnId,
             ets_copy = cfg_db:copy_to_ets()
            }.

exit_txn(#cfg_txn{ets_copy = EtsCopy}) ->
    catch ets:delete(EtsCopy),
    ok.


%% @doc commit the operations stored up in the configuration transaction
commit(#cfg_txn{ets_copy = Copy, ops = Ops}) ->
    ?DBG("cfg_txn: committing~n",[]),
    Fun = fun() ->
                  lists:foreach(fun({set, Path, Value}) ->
                                    cfg_db:insert_path_items(permanent, Path, Value);
                               (_) ->
                                    ok
                            end, Ops)
          end,
    cfg_db:transaction(Fun).

-spec get(#cfg_txn{}, cfg:path()) -> {ok, cfg:value()} | undefined.
get(#cfg_txn{ets_copy = Copy}, Path) ->
    case ets:lookup(Copy, Path) of
        [Value] ->
            {ok, Value};
        [] ->
            cfg_schema:lookup_default(Path)
    end.

% -spec set(#cfg_txn{}, [#cfg_schema{}], term()) -> ok | {error, String()}.
set(#cfg_txn{ets_copy = Copy, ops = Ops} = Txn, Path, Value) ->
    case cfg_db:check_conflict({ets, Copy}, Path, Value) of
        ok ->
            cfg_db:insert_path_items({ets, Copy}, Path, Value),
            ?DBG("ets content: ~p~n",[ets:tab2list(Copy)]),
            {ok, Txn#cfg_txn{ops = [{set, Path, Value} | Ops]}};
        {error, Reason} ->
            ?DBG(Reason),
            {error, Reason}
    end.

list_keys(#cfg_txn{ets_copy = Copy}, Path) ->
    ets:select(Copy, [{#cfg{path = Path ++ ['$1'], _ = '_'}, [], ['$1']}]).



