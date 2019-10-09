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
-export([name/1, desc/1, children/3, action/1, node_type/1]).

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
%% The callbacks needed for the generic expander to know enough
%% about our #cfg_schema{}s
%% --------------------------------------------------------------------
name(#cfg_schema{name = Name}) -> Name.

desc(#cfg_schema{desc = Desc}) -> Desc.

children(#cfg_schema{node_type = container, name = Name, path = Path, children = Cs}, Txn, _AddListItems) ->
    Children = expand_children(Cs, Txn),
    insert_full_path(Children, Path ++ [Name]);
children(#cfg_schema{node_type = list, path = Path, name = Name} = S, Txn, AddListItems) ->
    %% Children for list items are the list keys, and which one
    %% depends on how far we got in gathering the list keys. We can
    %% abuse the key_values field to track how many list keys we have,
    %% and re-use the same #cfg_schema{} list item for all the list
    %% item "children" so we still have it around for the real
    %% children.
    KeysSoFar = length(S#cfg_schema.key_value),
    KeysNeeded = size(S#cfg_schema.key),
    if KeysSoFar == KeysNeeded ->
            %% Now we have all the keys return the real child list.
            %% FIXME - remove the list keys from this list
            Children = expand_children(S#cfg_schema.children, Txn),
            insert_full_path(Children, Path ++ [Name]);
       true ->
            %% First time: Needed = 2, SoFar == 0, element = 1
            %% 2nd time:   Needed = 2, SoFar = 1, element = 2
            %% Last time:  Needed = 2, SoFar = 2
            NextKey = element(KeysSoFar + 1, S#cfg_schema.key),
            Keys = [NextKey | S#cfg_schema.key_value],
            Template = S#cfg_schema{key_value = Keys},
            ListKeys = cfg_txn:list_keys(Txn, S#cfg_schema.path),
            %% This is all the keys. We need to only show the current
            %% level, only unique values, and only items where the
            %% previous key parts match
            %%
            %% We don't have all this: the path isn't filled in, and
            %% we don't have the previous key values
            %% FIXME: Fill the path in
            %% FIXME: include previous key parts somewhere
            io:format("TEMPLATE ~p~n",[Template]),

            %% The goal here is to return the set of possible values at this point, plus soemthing that will prompt for a new list item
            %% So we need to just convince the menu thingy we are a normal list of children, and we need to keep enough blah around so we can carry one afterwards
            case AddListItems of
                true ->
                    [Template#cfg_schema{node_type = new_list_item}];
                false ->
                    []
            end
    end;
children(_, _, _) ->
    [].


action(_) -> fun(_) -> ok end. % Not used for config tree items, but provide default

node_type(#cfg_schema{node_type = Type}) -> Type.


expand_children(F, Arg) when is_function(F) ->
    case erlang:fun_info(F, arity) of
        {arity, 0} -> F();
        {arity, 1} -> F(Arg)
    end;
expand_children(L, _Arg) when is_list(L) -> L;
expand_children(_, _) -> [].

insert_full_path(Children, Path) ->
    lists:map(fun(#cfg_schema{} = S) ->
                      S#cfg_schema{path = Path}
              end, Children).

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

