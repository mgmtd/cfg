%%% @author Sean Hinde <sean@Seans-MacBook.local>
%%% @copyright (C) 2019, Sean Hinde
%%% @doc API into schema driven configuration database
%%%
%%% @end
%%% Created : 11 Aug 2019 by Sean Hinde <sean@Seans-MacBook.local>

-module(cfg).

-include("cfg.hrl").

-export([init/2, remove_db/2, load_schema/1, load_schema/2, subscribe/2, subscribe/3]).

%% Functions for users / application writers / schema designers to create
%% nodes in their configuration tree
-export([container/3, container/4,
         list/4, list/5,
         leaf_list/3, leaf_list/4,
         leaf/4, leaf/5
        ]).


-export([transaction/0, exit_transaction/1, set/3, show/2, commit/1]).

%% API towards current configuration
-export([lookup/1]).

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

remove_db(Backend, Opts) ->
    cfg_db:remove_db(Backend, Opts).

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
    ?DBG("Setting path ~p to value ~p in Txn ~p~n",[SchemaPath, Value, Txn]),
    cfg_txn:set(Txn, SchemaPath, Value).

show(Txn, SchemaPath) ->
    Tree = cfg_txn:get_tree(Txn, SchemaPath),
    {ok, Tree}.

commit(Txn) ->
    cfg_server:commit(Txn).

subscribe(Path, Pid) ->
    subscribe(Path, Pid, []).

subscribe(Path, Pid, Opts) ->
    cfg_server:subscribe(Path, Pid, Opts).

%% @doc read current config at path at a single level outside of a
%% transaction. For a leaf this will be the configured value of the
%% leaf or its default if provided in the schema. For a list item this
%% will be the list of list keys, for a container item or full path to
%% a list item this will be a list of child nodes.
lookup(Path) ->
    SchemaPath = lists:filter(fun(El) -> not is_tuple(El) end, Path),
    case cfg_schema:lookup(SchemaPath) of
        false ->
            {error, unkown_schema_path};
        #{node_type := list, key_names := Keys} ->
            case lists:last(Path) of
                ListKey when is_tuple(ListKey) ->
                    %% It's a full path to a list entry, return the
                    %% names of the children of the list key
                    Cs = cfg_schema:children(SchemaPath),
                    {ok, lists:map(fun(#{name := Name}) -> Name end, Cs)};
                _ ->
                    %% User looked up the list key itself, return the list keys
                    Pattern = erlang:make_tuple(length(Keys), '_'),
                    {ok, cfg_db:list_keys(Path)}
            end;
        #{node_type := container} ->
            Cs = cfg_schema:children(SchemaPath),
            {ok, lists:map(fun(#{name := Name}) -> Name end, Cs)};
        #{node_type := Leaf, default := Default} when Leaf == leaf;
                                                      Leaf == leaf_list ->
            case cfg_db:lookup(Path) of
                [#cfg{value = Value}] ->
                    {ok, Value};
                [] when Default == undefined ->
                    {ok, undefined};
                [] ->
                    {ok, Default}
            end
    end.



schema_list_to_path(SchemaItems) ->
    lists:map(fun(#{rec_type := schema, name := Name}) -> Name end, SchemaItems).

schema_list_to_path([#{rec_type := schema} = Last], Acc) ->
    {Last, lists:reverse(Acc)};
schema_list_to_path([#{rec_type := schema, name := Name} | Ss], Acc) ->
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
    #{rec_type => schema,
      node_type => container,
      name => Name,
      desc => Desc,
      children => Children,
      opts => Opts
     }.


list(Name, Desc, Key, Children) ->
    list(Name, Desc, Key, Children, []).

list(Name, Desc, KeyNames, Children, Opts) when is_list(KeyNames) ->
    #{rec_type => schema,
      node_type => list,
      name => Name,
      desc => Desc,
      children => Children,
      key_names => KeyNames,
      opts => Opts
     }.

leaf_list(Name, Desc, Type) ->
    leaf_list(Name, Desc, Type, []).

leaf_list(Name, Desc, Type, Opts) ->
    #{rec_type => schema,
      node_type => leaf_list,
      name => Name,
      desc => Desc,
      type => Type,
      opts => Opts
     }.

leaf(Name, Desc, Type, Default) ->
    leaf(Name, Desc, Type, Default, []).

leaf(Name, Desc, Type, Default, Opts) ->
    #{rec_type => schema,
      node_type => leaf,
      name => Name,
      desc => Desc,
      type => Type,
      default => Default,
      opts => Opts
     }.

