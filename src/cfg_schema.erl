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

-export([init/0, install_schema/1, install_schema/2, parse/2,
        remove_schema/0, remove_schema/1]).

-export([children/1, lookup/1, lookup_path/1, lookup_default/1]).

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

remove_schema() ->
    remove_schema(default).

remove_schema(NS) ->
    case ets:lookup(cfg_schema_ns, NS) of
        [{NS, Tid}] ->
            ets:delete(Tid),
            ets:delete(cfg_schema_ns, NS);
        [] ->
            ok
    end.

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
    SchemaPath = lists:filter(fun(P) -> not is_tuple(P) end, Path),
    case ets:lookup(cfg_schema_ns, NS) of
        [{_, Ets}] ->
            case ets:lookup(Ets, SchemaPath) of
                [#cfg_schema{} = Res] ->
                    schema_to_map(Res);
                [] ->
                    false
            end;
        [] ->
            false
    end;
lookup(Path) ->
    %% ?DBG("lookup at path ~p~n",[Path]),
    lookup([{ns, default}|Path]).

lookup_default(Path) ->
    case lookup(Path) of
        false ->
            {error, path_not_in_schema};
        #{node_type := NodeType, default := Default} when ?is_leaf(NodeType) ->
            {ok, Default};
        _ ->
            {error, missing_default}
    end.

children([{ns, NS} | Path]) ->
     case ets:lookup(cfg_schema_ns, NS) of
        [{_, Ets}] ->
             SchemaPath = lists:filter(fun(P) -> not is_tuple(P) end, Path),
             Recs = ets:match_object(Ets, #cfg_schema{path = SchemaPath ++ ['_'], _ = '_'}),
             lists:map(fun(R) -> schema_to_map(R) end, Recs);
        [] ->
            []
     end;
children(Path) ->
    %% ?DBG("children at path ~p~n",[Path]),
    children([{ns, default}|Path]).

%% @doc Given a path of the form ["server", "servers", {"S1"}, "port"]
%% return a list of schema items for the same path with any data after
%% a leaf list considered to be a value.
lookup_path(Path) ->
    lookup_path(Path, [], []).

lookup_path([P | Ps], PathSoFar, [#{node_type := list} = L | Acc]) when is_tuple(P) ->
    case lookup(PathSoFar) of
        #{} = Map ->
            ListItem = L#{key_values := tuple_to_list(P)},
            lookup_path(Ps, PathSoFar, [ListItem | Acc]);
        false ->
            {error, {unknown_path, PathSoFar}}
    end;
lookup_path([P | Ps], PathSoFar, Acc) ->
    NextPath = PathSoFar ++ [P],
    case lookup(NextPath) of
        #{node_type := Leaf} = Map when ?is_leaf(Leaf)  ->
            {ok, lists:reverse([Map | Acc]), Ps};
        #{} = Map ->
            lookup_path(Ps, NextPath, [Map | Acc]);
        false ->
            {error, {unknown_path, PathSoFar}}
    end;
lookup_path([], _, Acc) ->
    {ok, lists:reverse(Acc), ""}.


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

expand_nodes([#{rec_type := schema, node_type := container,
                name := Name, children := Cs} = T|Ts], Path, Acc) ->
    Children = eval_children(Cs),
    ThisPath = [Name | Path],
    SchemaEntry = map_to_schema(T),
    T1 = SchemaEntry#cfg_schema{children = expand_nodes(Children, ThisPath, []),
                                path = lists:reverse(ThisPath)},
    expand_nodes(Ts, Path, [T1|Acc]);
expand_nodes([#{rec_type := schema, node_type := list,
                children := Cs, name := Name} = T|Ts], Path, Acc) ->
    Children = eval_children(Cs),
    ThisPath = [Name | Path],
    SchemaEntry = map_to_schema(T),
    T1 = SchemaEntry#cfg_schema{children = expand_nodes(Children, ThisPath, []),
                                path = lists:reverse(ThisPath)},
    expand_nodes(Ts, Path, [T1|Acc]);
expand_nodes([#{rec_type := schema, node_type := Leaf, name := Name} = T|Ts],
             Path, Acc) when Leaf == leaf; Leaf == leaf_list ->
    SchemaEntry = map_to_schema(T),
    ThisPath = [Name | Path],
    T1 = SchemaEntry#cfg_schema{path = lists:reverse(ThisPath)},
    expand_nodes(Ts, Path, [T1|Acc]);
expand_nodes([], _Path, Acc) ->
    lists:reverse(Acc).

eval_children(Cs) when is_list(Cs) ->
    Cs;
eval_children(Fn) when is_function(Fn) ->
    Fn();
eval_children({M,F}) ->
    M:F().

map_to_schema(#{rec_type := schema,
                node_type := container, name := Name} = M) ->
    #cfg_schema{node_type = container,
                name = Name,
                desc = maps:get(desc, M, ""),
                opts = extra_map_keys(M)
               };
map_to_schema(#{rec_type := schema, node_type := list,
                name := Name, key_names := KeyNames} = M) ->
    #cfg_schema{node_type = list,
                name = Name,
                desc = maps:get(desc, M, ""),
                key_names = KeyNames,
                opts = extra_map_keys(M)
               };
map_to_schema(#{rec_type := schema, node_type := Leaf, name := Name,
                type := Type} = M) when Leaf == leaf;
                                        Leaf == leaf_list ->
    #cfg_schema{node_type = Leaf,
                name = Name,
                desc = maps:get(desc, M, ""),
                type = Type,
                opts = extra_map_keys(M)
               }.



%% Put any map keys from the user provided schema definitions that
%% don't fit in the #cfg_schema{} standard fields into a new map for
%% the opts field of #cfg_schema
extra_map_keys(Map) ->
    StandardKeys = record_info(fields, cfg_schema),
    MapKeys = maps:keys(Map),
    ExtraKeys = MapKeys -- [rec_type | StandardKeys],
    lists:foldl(fun(Key, OptMap) ->
                        #{Key := Value} = Map,
                        maps:put(Key, Value, OptMap)
                end, #{}, ExtraKeys).

schema_to_map(#cfg_schema{opts = Opts, path = Path} = S) ->
    Map = #{rec_type => schema,
            path => Path,
            node_type => S#cfg_schema.node_type,
            name => S#cfg_schema.name,
            desc => S#cfg_schema.desc,
            type => S#cfg_schema.type,
            key_names => S#cfg_schema.key_names,
            key_values => S#cfg_schema.key_values,
            children => fun() -> children(Path) end
           },
    maps:merge(Map, Opts).
