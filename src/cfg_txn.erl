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

-export([new/0, exit_txn/1, get/2, set/3, commit/1]).

new() ->
    TxnId = erlang:unique_integer(),
    #cfg_txn{txn_id = TxnId,
             ets_copy = cfg_db:copy_to_ets()
            }.

exit_txn(#cfg_txn{ets_copy = EtsCopy}) ->
    catch ets:delete(EtsCopy),
    ok.


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
    ets_insert_path_items(Copy, Path, Value, []),
    io:format("~p~n",[ets:tab2list(Copy)]),
    Txn#cfg_txn{ops = [{set, Path, Value} | Ops]}.


%% Insert all the database items required for a single configuration
%% item.  One entry is required for each level in the path plus a few
%% more for list items.
%%
%% If the entry already exists, but is of a different type that's bad:
%% it means the schema used to create the database was different to
%% the schema used to parse the command. Bail if this happens.
%%
%% List items. Are awkward. We need to generate several database
%% entries for the list item, one for each of the list keys, and
%% potentially one for a value. We need these for easy lookup /
%% subscribe etc.
%%
%% e.g. {set, ["a",{"b","c"},"name"], "Val"} leads to these database entries:
%%
%% #cfg{node_type = list, path = ["a"], value = {"keyb_name", "keyc_name"}}
%% #cfg{node_type = leaf, path = ["a", {"b", "c"}, "keyb_name"], value = "b"}
%% #cfg{node_type = leaf, path = ["a", {"b", "c"}, "keyc_name"], value = "c"}
%% #cfg{node_type = leaf, path = ["a", {"b", "c"}, "name"],     value = "Val"}
%%
%% The path we get here is a list of schema items, normally not
%% including any values. A few options to geth the list key values to here:
%%
%% 1. Have cfg_lookup create multiple entries for all the parts of a list entry
%% 2. Include the values of list keys in the #cfg_schema{node_type=list} item
%% 3. Hm
%%
%% Insert a single entry in the database.
-spec ets_insert_path_items(ets:tid(), [#cfg_schema{}], term(), list()) -> ok.
ets_insert_path_items(Ets, [I | Is], Value, Path) ->
    case I of
        #cfg_schema{node_type = container, name = Name} ->
            FullPath = Path ++ [Name],
            Cfg = schema_to_cfg(I, FullPath, undefined),
            ets:insert(Ets, Cfg),
            ets_insert_path_items(Ets, Is, Value, FullPath);
        #cfg_schema{node_type = list, name = Name, key = Key, key_value = KV} ->
            FullPath = Path ++ [Name],
            ListItemCfg = schema_to_cfg(I, FullPath, Key),
            ets:insert(Ets, ListItemCfg),
            ListItemsPath = FullPath ++ [KV],
            insert_list_keys(Ets, ListItemsPath, I),
            ets_insert_path_items(Ets, Is, Value, ListItemsPath);
        #cfg_schema{node_type = Leaf, name = Name} when Leaf == leaf;
                                                        Leaf == leaf_list ->
            FullPath = Path ++ [Name],
            Cfg = schema_to_cfg(I, FullPath, Value),
            ets:insert(Ets, Cfg)
    end.

insert_list_keys(Ets, Path, #cfg_schema{key = Key, key_value = KV} = I) ->
    KeyNames = tuple_to_list(Key),
    KeyValues = tuple_to_list(KV),
    NVPairs = lists:zip(KeyNames, KeyValues),
    lists:foreach(fun({Name, Value}) ->
                          Cfg = #cfg{name = Name,
                                     path = Path ++ [Name],
                                     node_type = leaf,
                                     value = Value},
                          ets:insert(Ets, Cfg)
                  end, NVPairs).

commit(#cfg_txn{ets_copy = Copy, ops = Ops}) ->
    case cfg_db:apply_ops(Ops) of
        ok ->
            ets:delete(Copy),
            ok;
        Err ->
            ets:delete(Copy),
            Err
    end.


schema_to_cfg(#cfg_schema{} = C, Path, Value) ->
    #cfg{node_type = C#cfg_schema.node_type,
         name = C#cfg_schema.name,
         path = Path,
         value = Value
        }.

