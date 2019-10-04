%%%-------------------------------------------------------------------
%%% @author Sean Hinde <sean@Seans-MacBook.local>
%%% @copyright (C) 2019, Sean Hinde
%%% @doc Configuration session transaction handler
%%%
%%% @end
%%% Created : 18 Sep 2019 by Sean Hinde <sean@Seans-MacBook.local>
%%%-------------------------------------------------------------------
-module(cfg_txn).

-record(cfg_txn,
        {
         txn_id,
         ops = [],
         ets_copy
         }).

-type txn() :: #cfg_txn{}.

-export_type([txn/0]).

-export([new/0, exit_txn/1, get/2, set/3, commit/1]).

new() ->
    TxnId = erlang:monotonic_time(),
    #cfg_txn{txn_id = TxnId,
             ets_copy = cfg_db:copy_to_ets()
            }.

exit_txn(#cfg_txn{ets_copy = EtsCopy}) ->
    catch ets:delete(EtsCopy),
    ok.


-spec get(#cfg_txn{}, cfg:path()) -> {ok, cfg:value()} | undefined.
get(#cfg_txn{ets_copy = Copy}, Path) ->
    case cfg_db:lookup(Copy, Path) of
        {ok, Value} ->
            {ok, Value};
        false ->
            cfg_schema:lookup_default(Path)
    end.

set(#cfg_txn{ets_copy = Copy, ops = Ops} = Txn, Path, Value) ->
    case cfg_db:insert(Copy, Path, Value) of
        ok ->
            Txn#cfg_txn{ops = [{set, Path, Value} | Ops]};
        {error, _Reason} = Err ->
            throw(Err)
    end.

commit(#cfg_txn{ets_copy = Copy, ops = Ops}) ->
    case cfg_db:apply_ops(Ops) of
        ok ->
            ets:delete(Copy),
            ok;
        Err ->
            ets:delete(Copy),
            Err
    end.
