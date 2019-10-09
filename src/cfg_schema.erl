%%%-------------------------------------------------------------------
%%% @author Sean Hinde <sean@Seans-MacBook.local>
%%% @copyright (C) 2019, Sean Hinde
%%% @doc Schema definition handling for configuration database
%%%
%%% @end
%%% Created :  6 Sep 2019 by Sean Hinde <sean@Seans-MacBook.local>
%%%-------------------------------------------------------------------

%%--------------------------------------------------------------------
%% PURPOSE
%%
%% The schemas managed by this module perform the following functions:
%% 1. Validation of entries into the configuration database
%% 2. Guide cli expansion
%% 3. Map a path to its type and other meta data
%%
%%--------------------------------------------------------------------
-module(cfg_schema).

-export([init/0, install_schema/1, install_schema/2, parse/2, lookup/1]).

-include("cfg.hrl").

%% @doc Initialise schema ets tables.
%%
%% 1. Create a public one for mapping Namespace to an ets table holding
%%    the tree for that namespace.
%%
%% 2. Create a second table for config trees with no specified namespace
-spec init() -> ets:tid().
init() ->
    case ets:info(cfg_schema_ns, size) of
        undefined ->
            ets:new(cfg_schema_ns, [public, named_table]),
            schema_ets(default);
        0 ->
            schema_ets(default),
            cfg_schema_ns;
        N when is_integer(N) ->
            ets:foldl(fun({_Ns, Tid}, _Acc) ->
                              ets:delete_all(Tid) end,
                      unused, cfg_schema_ns),
            ets:delete_all(cfg_schema_ns),
            schema_ets(default),
            cfg_schema_ns
    end.

install_schema(ParsedSchema) ->
    install_schema(default, ParsedSchema).

install_schema(NS, ParsedSchema) ->
    SchemaEts = schema_ets(NS),
    install_tree(SchemaEts, ParsedSchema).


install_tree(Ets, [Node|Nodes]) ->
    case Node of
        #cfg_schema{node_type = container, children = Cs} ->
            Children = eval_children(Cs),
            insert_node(Ets, Node),
            install_tree(Ets, Children),
            install_tree(Ets, Nodes);

        #cfg_schema{node_type = list, children = Cs} ->
            Children = eval_children(Cs),
            %% FIXME. Somewhere we should assert the list keys also
            %% exist as children
            insert_node(Ets, Node),
            install_tree(Ets, Children),
            install_tree(Ets, Nodes);

        #cfg_schema{node_type = Leaf} when Leaf == leaf; Leaf == leaf_list ->
            insert_node(Ets, Node),
            install_tree(Ets, Nodes)
    end;
install_tree(_, []) ->
    ok.

insert_node(Ets, #cfg_schema{} = Node) ->
    ets:insert(Ets, Node#cfg_schema{children = []}).

schema_ets(NS) ->
    case ets:lookup(cfg_schema_ns, NS) of
        [{NS, Tid}] ->
            Tid;
        [] ->
            Tid = init_schema_tab(),
            ets:insert(cfg_schema_ns, {NS, Tid}),
            Tid
    end.

init_schema_tab() ->
    ets:new(cfg_schema, [{keypos, #cfg_schema.path}, ordered_set]).


lookup([{ns, NS}|Path]) ->
    case ets:lookup(cfg_schema_ns, NS) of
        [{_, Ets}] ->
            case ets:lookup(Ets, Path) of
                [#cfg_schema{} = Res] ->
                    Res;
                [] ->
                    false
            end;
        [] ->
            false
    end;
lookup(Path) ->
    lookup([{ns, default}|Path]).


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

expand_nodes([#cfg_schema{node_type = container, children = Cs} = T|Ts], Path, Acc) ->
    Children = eval_children(Cs),
    ThisPath = [T#cfg_schema.name| Path],
    T1 = T#cfg_schema{children = expand_nodes(Children, ThisPath, []), path = lists:reverse(ThisPath)},
    expand_nodes(Ts, Path, [T1|Acc]);
expand_nodes([#cfg_schema{node_type = list, children = Cs} = T|Ts], Path, Acc) ->
    Children = eval_children(Cs),
    ThisPath = [T#cfg_schema.name| Path],
    T1 = T#cfg_schema{children = expand_nodes(Children, ThisPath, []), path = lists:reverse(ThisPath)},
    expand_nodes(Ts, Path, [T1|Acc]);
expand_nodes([#cfg_schema{node_type = leaf} = T|Ts], Path, Acc) ->
    T1 = T#cfg_schema{path = lists:reverse([T#cfg_schema.name|Path])},
    expand_nodes(Ts, Path, [T1|Acc]);
expand_nodes([#cfg_schema{node_type = leaf_list} = T|Ts], Path, Acc) ->
    T1 = T#cfg_schema{path = lists:reverse([T#cfg_schema.name|Path])},
    expand_nodes(Ts, Path, [T1|Acc]);
expand_nodes([], _Path, Acc) ->
    lists:reverse(Acc).

eval_children(Cs) when is_list(Cs) ->
    Cs;
eval_children(Fn) when is_function(Fn) ->
    Fn();
eval_children({M,F}) ->
    M:F().


