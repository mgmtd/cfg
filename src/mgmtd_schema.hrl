
-ifdef(DEBUG).
-define(DBG(DATA), io:format(user, "[~p:~p] ~p~n",[?MODULE, ?LINE, DATA])).
-define(DBG(FORMAT, ARGS), io:format(user, "[~p:~p] " ++ FORMAT,[?MODULE, ?LINE] ++ ARGS)).
-else.
-define(DBG(DATA), ok).
-define(DBG(FORMAT, ARGS), ok).
-endif.

-type ns() :: atom().
-type path_node() :: atom() | tuple().
-type item_path() :: [path_node()].
-type schema_path() :: [atom()].
-type node_type() :: container | leaf | list | leaf_list.

-define(is_leaf(NodeType), NodeType == leaf orelse NodeType == leaf_list).
-define(DEFAULT_NS, mgmtd_schema_default_ns).

%% Record stored in schema ets tables. One table for each namespace
-record(schema,
        {path :: schema_path(),     % Full path to item in tree
         node_type :: node_type(),  % container | leaf | list | leaf_list
         name :: binary(),
         desc :: string(),
         type :: mgmtd:data_type(),
         default,
         key_names = [] :: [atom()], % {NodeName1, NodeName2, Nodename3} for lists
         data_callback :: atom(),
         min_elements,
         max_elements,
         pattern :: undefined | string(),
         mandatory = false :: boolean(),
         config = true :: boolean()}).

%% Record we store in the configuration database after validation against the schema.
-record(cfg,
        {
         path,
         name,
         node_type = container,
         value
        }).