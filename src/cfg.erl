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

%% The db schema is collected through callbacks into erlang code. The
%% Erlang program calls cfg:load_schema/2 with a function that
%% provides the tree of schema items via the generator functions in
%% this module

%%
%% The config schema is provided by the managed system as a tree of
%% #cfg{} records. Types supported are leaf values including:
%% #cfg{node_type = leaf,
%%      type = integer,
%%      restrictions = Fun(val)
%%     }
%%
%% Also supported are lists of nodes
%%  #cfg{node_type = list,
%%       key = {Node1, Node2, Node3}
%%       children = {M,F} | fun()
%%      }
%%
%% Lists of leafs:
%%  #cfg{node_type = leaf_list,
%%       type = integer | string
%%      }
%%
%% and further trees of types
%%  #cfg{node_type = container,
%%       children = [{Module, Func}]
%%      }

%% Set up the structure needed for the generic expander to know enough about our #tree{}s
getters() ->
    cli:getters(fun name/1, fun desc/1, fun children/1).

name(#tree{name = Name}) -> Name.

desc(#tree{desc = Desc}) -> Desc.

children(#tree{children = Cs}) -> Cs.


load_schema(Generators) ->
    load_schema(Generators, "http://ns").

load_schema(Generator, NameSpace) when is_function(Generator) ->
    load_schema([Generator], NameSpace);
load_schema({Mod, Func} = Generator, NameSpace) when is_atom(Mod), is_atom(Func) ->
    load_schema([Generator], NameSpace);
load_schema(Generators, NameSpace) when is_list(Generators) ->
    TreeList = parse_schema(Generators, NameSpace),
    lists:foreach(fun({NS, Tree}) ->
                          cfg_server:load_schema(NS, Tree)
                  end, TreeList).


parse_schema(Generators, NameSpace) ->
    lists:map(fun(Generator) ->
                      schema(Generator, NameSpace)
              end, Generators).

schema(Generator, NS) ->
    Nodes = call_generator(Generator),
    Tree = expand_nodes(Nodes),
    {NS, Tree}.

call_generator(Generator) when is_function(Generator) ->
    Generator();
call_generator({M, F}) ->
    M:F().

%% Expand a tree with Funs providing children into the full data structure.
%% Also annotate each node with its full path.
expand_nodes(Nodes) ->
    expand_nodes(Nodes, [], []).

expand_nodes([#tree{node_type = container, children = Cs} = T|Ts], Path, Acc) ->
    Children = eval_children(Cs),
    ThisPath = [T#tree.name| Path],
    T1 = T#tree{children = expand_nodes(Children, ThisPath, []), path = lists:reverse(ThisPath)},
    expand_nodes(Ts, Path, [T1|Acc]);
expand_nodes([#tree{node_type = list, children = Cs} = T|Ts], Path, Acc) ->
    Children = eval_children(Cs),
    ThisPath = [T#tree.name| Path],
    T1 = T#tree{children = expand_nodes(Children, ThisPath, []), path = lists:reverse(ThisPath)},
    expand_nodes(Ts, Path, [T1|Acc]);
expand_nodes([#tree{node_type = leaf} = T|Ts], Path, Acc) ->
    T1 = T#tree{path = lists:reverse([T#tree.name|Path])},
    expand_nodes(Ts, Path, [T1|Acc]);
expand_nodes([#tree{node_type = leaf_list} = T|Ts], Path, Acc) ->
    T1 = T#tree{path = lists:reverse([T#tree.name|Path])},
    expand_nodes(Ts, Path, [T1|Acc]);
expand_nodes([], _Path, Acc) ->
    lists:reverse(Acc).

eval_children(Cs) when is_list(Cs) ->
    Cs;
eval_children(Fn) when is_function(Fn) ->
    Fn();
eval_children({M,F}) ->
    M:F().



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

