%%%-------------------------------------------------------------------
%%% @author Sean Hinde <sean@Seans-MacBook.local>
%%% @copyright (C) 2019, Sean Hinde
%%% @doc Configuration storage backend for mnesia
%%%
%%% @end
%%% Created :  1 Oct 2019 by Sean Hinde <sean@Seans-MacBook.local>
%%%-------------------------------------------------------------------
-module(cfg_backend_mnesia).

-include("cfg.hrl").

-export([init/1]).

-export([copy_to_ets/0]).

-export([transaction/1,
         read/1,
         write/1,
         delete/1,
         first/0,
         next/1
        ]).

%%--------------------------------------------------------------------
%% API callbacks
%%--------------------------------------------------------------------

%% @doc initialise the mnesia backend in a standard directory relative
%% to the working directory of the erlang node ("database")
-spec init(proplists:proplist()) -> ok.
init(Opts) ->
    Dir = proplists:get_value(directory, Opts, "database"),
    Nodes = proplists:get_value(nodes, Opts, [node()]),
    init_db(Dir, Nodes).

%% @doc init mnesia in a directory at Path on Nodes.
-spec init_db(file:filename(), [node()]) -> ok.
init_db(Path, Nodes) ->
    ok = filelib:ensure_dir(Path),
    ok = application:set_env(mnesia, mnesia_dir, Path),
    create_schema(Nodes),
    ok = start_mnesia(),
    ok = create_table(Nodes),
    ok = mnesia:wait_for_tables([cfg], 30000).

transaction(Fun) ->
    case mnesia:transaction(Fun) of
        {atomic, ok} ->
            ok;
        Err ->
            ?DBG("Transaction errro ~p",[Err]),
            {error, "FAIL"}
    end.

read(Key) ->
    mnesia:read(cfg, Key).

write(#cfg{} = Cfg) ->
    mnesia:write(cfg, Cfg, write).

delete(Key) ->
    mnesia:delete(cfg, Key).

first() ->
    mnesia:first(cfg).

next(Key) ->
    mnesia:next(cfg, Key).

-spec copy_to_ets() -> ets:tid().
copy_to_ets() ->
    Ets = ets:new(cfg_txn, [ordered_set, {keypos, #cfg.path}]),
    Fun = fun() -> mnesia:match_object(#cfg{_ = '_'}) end,
    {atomic, Rows} = mnesia:transaction(Fun),
    lists:foreach(fun(Row) -> ets:insert(Ets, Row) end, Rows),
    Ets.

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------
create_schema(Nodes) ->
    case catch mnesia:create_schema(Nodes) of
        ok ->
            ok;
        {error, {_, {already_exists, _}}} ->
            ok
    end.

start_mnesia() ->
    case application:start(mnesia) of
        ok ->
            ok;
        {error,{already_started,mnesia}} ->
            ok;
        Else ->
            Else
    end.

create_table(Nodes) ->
    case mnesia:create_table(cfg, [{disc_copies, Nodes},
                                   {type, ordered_set},
                                   {attributes, record_info(fields, cfg)}]) of
        {atomic, ok} ->
            ok;
        {aborted, {already_exists, _}} ->
            ok
    end.
