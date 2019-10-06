%%% @author Sean Hinde <sean@Seans-MacBook.local>
%%% @copyright (C) 2019, Sean Hinde
%%% @doc API into schema driven configuration database
%%%
%%% @end
%%% Created : 11 Aug 2019 by Sean Hinde <sean@Seans-MacBook.local>

-module(cfg).

-include("cfg.hrl").

-export([init/2, load_schema/1, load_schema/2]).

%% Functions needed to extract items from our schema records.
-export([name/1, desc/1, children/1, action/1, node_type/1]).

%% Functions to create nodes in the configuration tree
-export([container/3, container/4,
         list/4, list/5,
         leaf_list/3, leaf_list/4,
         leaf/3, leaf/4
        ]).


-export([transaction/0, exit_transaction/1, set/3]).

-export_type([value/0, yang_value/0]).

-type yang_value() :: {int32, integer()}
                    | {uint16, integer()}
                    | {uint32, integer()}
                    | {uint64, integer()}
                    | {decimal64, integer()}
                    | {boolean, boolean()}
                    | {string, string()}
                    | {enumeration, string()}.

-type value() :: {integer, integer()}
               | {string, string()}
               | {boolean, boolean()}
               | {enumeration, string()}.

%% The db schema is collected through callbacks into erlang code. The
%% Erlang program calls cfg:load_schema/2 with a function that
%% provides the tree of schema items via the generator functions in
%% this module

-spec init(cfg_db:backend(), list()) -> ok.
init(Backend, Opts) when Backend == mnesia ->
    cfg_db:init(Backend, Opts).

%%--------------------------------------------------------------------
%% Set up the structure needed for the generic expander to know enough
%% about our #cfg_schema{}s
%% --------------------------------------------------------------------
name(#cfg_schema{name = Name}) -> Name.

desc(#cfg_schema{desc = Desc}) -> Desc.

children(#cfg_schema{children = Cs}) -> Cs.

action(_) -> fun(_) -> ok end. % Not used for config tree items, but provide default

node_type(#cfg_schema{node_type = Type}) -> Type.

%%--------------------------------------------------------------------
%% Configuration session transaction API
%%
%%
%% @doc Long lived transaction started e.g. when user enters configuration
%% mode in the CLI. Creates a copy of the configuration for making
%% transaction local changes
%% --------------------------------------------------------------------
-spec transaction() -> cfg_txn:txn().
transaction() ->
    cfg_txn:new().

exit_transaction(Txn) ->
    cfg_txn:exit_txn(Txn).

set(Txn, SchemaPath, Value) ->
    io:format("Setting path ~p to value ~p in Txn ~p~n",[SchemaPath, Value, Txn]),
    cfg_txn:set(Txn, SchemaPath, Value),
    {ok, "ok", Txn}.

schema_list_to_path(SchemaItems) ->
    lists:map(fun(#cfg_schema{name = Name}) -> Name end, SchemaItems).

schema_list_to_path([#cfg_schema{} = Last], Acc) ->
    {Last, lists:reverse(Acc)};
schema_list_to_path([#cfg_schema{name = Name} | Ss], Acc) ->
    schema_list_to_path(Ss, [Name | Acc]).


%%--------------------------------------------------------------------
%% @doc load a list of #cfg_schema{} trees into ets storage
%%--------------------------------------------------------------------
load_schema(Generators) ->
    load_schema(Generators, default).

load_schema(Generator, NameSpace) when is_function(Generator) ->
    load_schema([Generator], NameSpace);
load_schema({Mod, Func} = Generator, NameSpace) when is_atom(Mod), is_atom(Func) ->
    load_schema([Generator], NameSpace);
load_schema(Generators, NameSpace) when is_list(Generators) ->
    TreeList = cfg_schema:parse(Generators, NameSpace),
    lists:foreach(fun({NS, Tree}) ->
                          cfg_server:load_schema(NS, Tree)
                  end, TreeList).



%%--------------------------------------------------------------------
%% User facing API to create configuration schema nodes of the 4 types
%%--------------------------------------------------------------------
-spec container(Name::string(), Desc::string(), Children::list(term())) ->
                       #cfg_schema{}.
container(Name, Desc, Children) ->
    container(Name, Desc, Children, []).

container(Name, Desc, Children, Opts) ->
    #cfg_schema{node_type = container,
                name = Name,
                desc = Desc,
                children = Children,
                opts = Opts
               }.


list(Name, Desc, Key, Children) ->
    list(Name, Desc, Key, Children, []).

list(Name, Desc, Key, Children, Opts) when is_tuple(Key) ->
    #cfg_schema{node_type = list,
                name = Name,
                desc = Desc,
                children = Children,
                key = Key,
                opts = Opts
               }.

leaf_list(Name, Desc, Type) ->
    leaf_list(Name, Desc, Type, []).

leaf_list(Name, Desc, Type, Opts) ->
    #cfg_schema{node_type = leaf_list,
                name = Name,
                desc = Desc,
                type = Type,
                opts = Opts
               }.

leaf(Name, Desc, Type) ->
    leaf(Name, Desc, Type, []).

leaf(Name, Desc, Type, Opts) ->
    #cfg_schema{node_type = leaf,
                name = Name,
                desc = Desc,
                type = Type,
                opts = Opts
               }.

