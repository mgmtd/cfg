%%%-------------------------------------------------------------------
%%% @author Sean Hinde <sean@Seans-MacBook.local>
%%% @copyright (C) 2019, Sean Hinde
%%% @doc Eunit tests for cfg_db
%%%
%%% @end
%%% Created : 15 Feb 2025 by Sean Hinde <sean@Seans-MacBook.local>
%%%-------------------------------------------------------------------
-module(mgmtd_db_test).

-include("../src/mgmtd_schema.hrl").

-include_lib("eunit/include/eunit.hrl").

setup_schema() ->
    mgmtd_sup:start_link(),
    %% ok = mgmtd_cfg_db:remove_db("test_db", mnesia),

    mgmtd:load_function_schema(fun() -> mgmtd_test_schema:cfg_schema() end),
    mgmtd_cfg_db:init("test_db", [{backend, mnesia}]),
    ok.

init_cfg() ->
    ok.

destroy_schema() ->
    mgmtd_schema:remove_schema(),
    mgmtd_cfg_db:remove_db("test_db", mnesia),
    ok.

subscription_test_() ->
    {setup,
     fun() ->
             setup_schema(),
             init_cfg()
     end,
     fun(_Pid) ->
            destroy_schema()
     end,
     fun(_Pid) ->
            %% test list item create
            SetPath = ["server", "servers", {"newlistitem"}, "port", "81"],
            {ok, SchemaPath} = mgmtd_schema:lookup_path(SetPath),
            %% ?debugFmt("Set Path ~p", [SchemaPath]),
            Txn = mgmtd:txn_new(),
            {ok, Txn2} = mgmtd:txn_set(Txn, SchemaPath),
            {ok, Txn3} = mgmtd:txn_commit(Txn2),
            ?'_assertEqual'(5, mnesia:table_info(cfg, size)),

            %% test list item delete
            DelPath = ["server", "servers", {"newlistitem"}],
            {ok, DelSchemaPath} = mgmtd_schema:lookup_path(DelPath),
            %% ?debugFmt("DEL Path ~p", [DelSchemaPath]),
            {ok, Txn4} = mgmtd:txn_delete(Txn3, DelSchemaPath),
            _ = mgmtd:txn_commit(Txn4),
            ?'_assertEqual'(0, mnesia:table_info(cfg, size))
     end}.

