%%% @author Sean Hinde <sean@Seans-MacBook.local>
%%% @copyright (C) 2019, Sean Hinde
%%% @doc API into schema driven configuration database
%%%
%%% @end
%%% Created : 11 Aug 2019 by Sean Hinde <sean@Seans-MacBook.local>

-module(cfg).

-include("cfg.hrl").

-export([load_schema/1, load_schema/2,
         getters/0,
         container/3, container/4,
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

%%--------------------------------------------------------------------
%% Set up the structure needed for the generic expander to know enough
%% about our #tree{}s
%% --------------------------------------------------------------------
getters() ->
    cli:getters(fun name/1, fun desc/1, fun children/1, fun action/1, fun node_type/1).

name(#tree{name = Name}) -> Name.

desc(#tree{desc = Desc}) -> Desc.

children(#tree{children = Cs}) -> Cs.

node_type(#tree{node_type = Type}) -> Type.

action(_) -> fun(_) -> ok end. % Not used for config tree items, but provide default

%%--------------------------------------------------------------------
%% Database API
%%--------------------------------------------------------------------
transaction() ->
    cfg_txn:new().

exit_transaction(Txn) ->
    cfg_txn:exit_txn(Txn).


set(Txn, #tree{} = Item, Value) ->
    io:format("Setting value~n"),
    cfg_txn:set(Txn, Item, Value),
    {ok, "ok", Txn}.


%%--------------------------------------------------------------------
%% Schema API
%%--------------------------------------------------------------------
load_schema(Generators) ->
    load_schema(Generators, "http://ns").

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
-spec container(Name::string(), Desc::string(), Children::list(term())) -> #tree{}.
container(Name, Desc, Children) ->
    container(Name, Desc, Children, []).

container(Name, Desc, Children, Opts) ->
    #tree{node_type = container,
          name = Name,
          desc = Desc,
          children = Children,
          opts = Opts
         }.


list(Name, Desc, Key, Children) ->
    list(Name, Desc, Key, Children, []).

list(Name, Desc, Key, Children, Opts) when is_tuple(Key) ->
    #tree{node_type = list,
          name = Name,
          desc = Desc,
          children = Children,
          key = Key,
          opts = Opts
         }.

leaf_list(Name, Desc, Type) ->
    leaf_list(Name, Desc, Type, []).

leaf_list(Name, Desc, Type, Opts) ->
    #tree{node_type = leaf_list,
          name = Name,
          desc = Desc,
          type = Type,
          opts = Opts
         }.

leaf(Name, Desc, Type) ->
    leaf(Name, Desc, Type, []).

leaf(Name, Desc, Type, Opts) ->
    #tree{node_type = leaf,
          name = Name,
          desc = Desc,
          type = Type,
          opts = Opts
         }.

