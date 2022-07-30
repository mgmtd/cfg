-module(mgmtd_schema_function).

-export([load/2]).

-include("../include/mgmtd.hrl").
-include("mgmtd_schema.hrl").

load(Fun, Opts) when is_function(Fun) ->
    NameSpace = maps:get(namespace, Opts, ?DEFAULT_NS),
    ets:new(NameSpace, [named_table, {keypos, #schema.path}]),
    load(Fun, [], NameSpace, true).

load(Fun, Path, Ns, IsConfig) ->
    lists:foreach(fun(Child) ->
        load_node(Child, Path, Ns, IsConfig)
    end, Fun()).

load_node(#container{name = Name, desc = Desc, config = Config0} = Node, Path, Ns, IsConfig) ->
    Config = inherited_config(Config0, IsConfig),
    Container =
        #schema{path = lists:reverse([Name | Path]),
                node_type = container,
                name = Name,
                desc = Desc,
                config = Config},
    %% io:format(user, "O - ~p~n", [lists:reverse(Path)]),
    true = ets:insert_new(Ns, Container),
    load(Node#container.children, [Name | Path], Ns, Config);
load_node(#list{name = Name, desc = Desc, key_names = KeyNames, config = Config0} = Node, Path, Ns, IsConfig) ->
    Config = inherited_config(Config0, IsConfig),
    assert_key_names(KeyNames, Node#list.children),
    LeafList =
        #schema{path = lists:reverse([Name | Path]),
                node_type = list,
                name = Name,
                key_names = KeyNames,
                data_callback = Node#list.data_callback,
                desc = Desc,
                min_elements = Node#list.min_elements,
                max_elements = Node#list.max_elements,
                config = Config},
    %io:format(user, "L - ~p~n", [lists:reverse(Path)]),
    true = ets:insert_new(Ns, LeafList),
    load(Node#list.children, [Name | Path], Ns, Config);
load_node(#leaf{name = Name, desc = Desc, type = Type, default = Default, config = Config0} = Node, Path, Ns, IsConfig) ->
    Config = inherited_config(Config0, IsConfig),
    Leaf =
        #schema{path = lists:reverse([Name | Path]),
                node_type = leaf,
                name = Name,
                type = Type,
                desc = Desc,
                default = Default,
                mandatory = Node#leaf.mandatory,
                config = Config},
    %io:format(user, "L - ~p~n", [lists:reverse(Path)]),
    true = ets:insert_new(Ns, Leaf);
load_node(#leaf_list{name = Name, desc = Desc, type = Type, config = Config0} = Node, Path, Ns, IsConfig) ->
    Config = inherited_config(Config0, IsConfig),
    LeafList =
        #schema{path = lists:reverse([Name | Path]),
                node_type = leaf_list,
                name = Name,
                type = Type,
                desc = Desc,
                min_elements = Node#leaf_list.min_elements,
                max_elements = Node#leaf_list.max_elements,
                config = Config},
    %io:format(user, "L - ~p~n", [lists:reverse(Path)]),
    true = ets:insert_new(Ns, LeafList).

assert_key_names(KeyNames, ChildrenFun) ->
    Children = ChildrenFun(),
    ChildNames = node_names(Children),
    case KeyNames -- ChildNames of
        [] -> ok;
        MissingNames ->
            io:format("Error, Missing list key entry"),
            error({missing_list_keys, MissingNames})
    end.

node_names(Children) ->
    lists:map(fun(Child) -> node_name(Child) end, Children).

node_name(#leaf{name = Name}) -> Name;
node_name(#leaf_list{name = Name}) -> Name;
node_name(#list{name = Name}) -> Name;
node_name(#container{name = Name}) -> Name.


%% Use yang rules here (Section 7.19.1 of RFC 6020):
%% If the top node does not specify a "config" statement, the default is
%%   "true".
%%
%%   If a node has "config" set to "false", no node underneath it can have
%%   "config" set to "true".
inherited_config(undefined, Existing) ->
    Existing;
inherited_config(false, true) ->
    io:format("Warning, ignored attempt to set config = true under config = false node\n"),
    false;
inherited_config(true, false) ->
    false;
inherited_config(true, _) ->
    true.