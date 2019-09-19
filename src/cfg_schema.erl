%%%-------------------------------------------------------------------
%%% @author Sean Hinde <sean@Seans-MacBook.local>
%%% @copyright (C) 2019, Sean Hinde
%%% @doc Schema definition handling for configuration database
%%%
%%% @end
%%% Created :  6 Sep 2019 by Sean Hinde <sean@Seans-MacBook.local>
%%%-------------------------------------------------------------------

%%--------------------------------------------------------------------
%% DESIGN
%%
%% The schema performs the following functions:
%% 1. Validation of entries into the configuration database
%% 2. Guide cli expansion
%% 3. Map a path to its type and other meta data
%%
%% Could either descend the tree from scratch each time we want to
%% reach a node, or store the expanded tree in an ets table.
%%
%% For now and until it becomes a problem stick with traversing the
%% original tree each time.
%% --------------------------------------------------------------------
-module(cfg_schema).

-export([init/0, load_schema/1, parse/2]).

-include("cfg.hrl").

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

load_schema(_Tree) ->
    ok.

parse(Generators, NameSpace) ->
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


