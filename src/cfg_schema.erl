%%%-------------------------------------------------------------------
%%% @author Sean Hinde <sean@Seans-MacBook.local>
%%% @copyright (C) 2019, Sean Hinde
%%% @doc Schema definition handling for configuration database
%%%
%%% @end
%%% Created :  6 Sep 2019 by Sean Hinde <sean@Seans-MacBook.local>
%%%-------------------------------------------------------------------
-module(cfg_schema).

-export([init/0, load_schema/1]).

-record(cfg_schema,
        {
         node_type = container,            % | leaf | list | leaf_list
         node_name,
         path,
         type,
         key = {},            % {NodeName1, NodeName2, Nodename3} for lists
         children            % [{M,F}, {M1,F1}, fun()] | {M,F} | fun()
        }).


init() ->
    case ets:info(cfg_schema, size) of
        undefined ->
            ets:new(cfg_schema, [public, named_table]);
        0 ->
            cfg_schema;
        N when is_integer(N) ->
            ets:delete_all(cfg_schema),
            cfg_schema
    end.

load_schema(Module) when is_atom(Module) ->
    load_schema([Module]);
load_schema(Modules) when is_list(Modules) ->
    lists:map(fun(Mod) ->
                      load_mod(Mod)
              end, Modules).

load_mod(Mod) ->
    ok.
