%%%-------------------------------------------------------------------
%% @doc mgmtd schema access
%% Global repository for management information schemas
%% Supports:
%%   * configuration and operational data schemas
%%   * Yang and JSON Schemas
%% @end
%%%-------------------------------------------------------------------
-module(mgmtd_schema).

-export([load_json_schema_file/1, load_json_schema_file/2, load_function_schema/2]).
-export([remove_schema/0, remove_schema/1]).
-export([lookup/1, lookup/2, get_default/1, get_default/2]).
-export([lookup_path/1]).
-export([children/1, children/2, children/3]).
-export([cast_value/2]).

-include("../include/mgmtd.hrl").
-include("mgmtd_schema.hrl").

%% @doc
%% Ideally we want to have the same internal representation of JSON schema
%% and Yang - a superset of the capabilities of both
%% with the same concepts in each dealt with by the same code
%%
%% Schema will be stored in an ets table for each namespace
%%
%% Mappings between JSON schema and Yang node types
%%
%% -----------------------------------
%% | Yang      | JSON Schema         |
%% -----------------------------------
%% | container | object              |
%% | list      | array of object     |
%% | leaf-list | array of basic type |
%% | leaf      | not object or array |
%% -----------------------------------
%%
%% min and max mappings
%% ------------------------------------------------------------------------
%% | Yang                                     | JSON Schema               |
%% ------------------------------------------------------------------------
%% | min-elements for lists and leaf-lists    | minItems for arrays
%% | max-elements for lists and leaf-lists    | maxItems for arrays
%% | length N or N..M for string types        | minLength and maxLength strings
%% | range N or N..M or multiple for integers | minimum and maximum
%%
%% JSON schema union types - not allowed. Could maybe map to Yang Choice?
%% For now, each leaf is only allowed to have a single type
%%
%% In Yang each subtree can be independentally configured to hold
%% either config or operational data. For JSON Schema this attribute
%% needs to be provided at load time.
%%
-spec load_json_schema_file(FilePath :: file:filename()) ->
                               ok | {error, Reason :: term()}.
load_json_schema_file(File) ->
    mgmtd_schema_json_draft4:load_file(File, #{config => true}).

-spec load_json_schema_file(FilePath :: file:filename(), Opts :: map()) ->
                               ok | {error, Reason :: term()}.
load_json_schema_file(File, Opts) ->
    mgmtd_schema_json_draft4:load_file(File, Opts).

load_function_schema(Fun, Opts) ->
    mgmtd_schema_function:load(Fun, Opts).

remove_schema() ->
    remove_schema(?DEFAULT_NS).

remove_schema(Ns) ->
    ets:delete(Ns).

-spec lookup(Path :: item_path()) -> map() | false.
lookup(Path) ->
    lookup(?DEFAULT_NS, Path).

-spec lookup(NameSpace :: ns(), Path :: item_path()) -> map() | false.
lookup(Ns, Path) ->
    SchemaPath = lists:filter(fun(P) -> not is_tuple(P) end, Path),
    case ets:lookup(Ns, SchemaPath) of
        [#schema{} = Res] ->
            schema_to_map(Res, show);
        [] ->
            false
    end.

get_default(Path) ->
    get_default(?DEFAULT_NS, Path).

get_default(Ns, Path) ->
    case lookup(Ns, Path) of
        false ->
            {error, path_not_in_schema};
        #{node_type := NodeType, default := Default} when ?is_leaf(NodeType) ->
            {ok, Default};
        _ ->
            {error, missing_default}
    end.

children(Path) ->
    children(?DEFAULT_NS, Path, show).

children(Path, CmdType) ->
    children(?DEFAULT_NS, Path, CmdType).

children(Ns, Path, CmdType) ->
    SchemaPath = lists:filter(fun(P) -> not is_tuple(P) end, Path),
    Recs = ets:match_object(Ns, #schema{path = SchemaPath ++ ['_'], _ = '_'}),
    lists:map(fun(R) -> schema_to_map(R, CmdType) end, Recs).

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
            [Val | _Pss] = Ps,
            {ok, lists:reverse([Map#{value => Val} | Acc])};
        #{} = Map ->
            lookup_path(Ps, NextPath, [Map | Acc]);
        false ->
            {error, {unknown_path, PathSoFar}}
    end;
lookup_path([], _, Acc) ->
    {ok, lists:reverse(Acc)}.

schema_to_map(#schema{path = Path} = S, CmdType) ->
    #{role => schema,
      path => Path,
      node_type => S#schema.node_type,
      name => S#schema.name,
      desc => S#schema.desc,
      type => S#schema.type,
      default => S#schema.default,
      key_names => S#schema.key_names,
      key_values => [],
      min_elements => S#schema.min_elements,
      max_elements => S#schema.max_elements,
      pattern => S#schema.pattern,
      mandatory => S#schema.mandatory,
      config => S#schema.config,
      data_callback => S#schema.data_callback,
      cmd_type => CmdType,
      children => fun(ChildPath) -> children(ChildPath, CmdType) end }.

%% @doc Validate Item against the schema stored at Path.
%% The item must be a value of a leaf or leaf-list
%%
-spec validate(Path :: schema_path(), Item :: term()) -> ok | {error, Reason :: term()}.
validate(Path, Item) ->
    ok.

cast_value(Path, Value) ->
    try case lists:last(Path) of
            #{node_type := leaf, type := Type} ->
                cast(Type, Value);
            #{node_type := leaf_list, type := Type} when is_list(Value) ->
                lists:map(fun(Val) ->
                            case cast(Type, Val) of
                                {ok, InternalVal} ->
                                    {ok, InternalVal};
                                Err ->
                                    throw(Err)
                            end
                        end, Value);
            _ ->
                {error, "Invalid path"}
        end
    catch error:Reason:_Trace ->
        {error, Reason}
    end.
        

cast({uint64, Range}, Token) ->
    FullRange = merge_ranges(uint64_range(), Range),
    cast_integer(Token, FullRange);
cast(uint64, Token) -> cast_integer(Token, uint64_range());

cast({uint32, Range}, Token) ->
    FullRange = merge_ranges(uint32_range(), Range),
    cast_integer(Token, FullRange);
cast(uint32, Token) -> cast_integer(Token, uint32_range());

cast({uint16, Range}, Token) ->
    FullRange = merge_ranges(uint16_range(), Range),
    cast_integer(Token, FullRange);
cast(uint16, Token) -> cast_integer(Token, uint16_range());

cast({uint8, Range}, Token) ->
    FullRange = merge_ranges(uint8_range(), Range),
    cast_integer(Token, FullRange);
cast(uint8, Token) -> cast_integer(Token, uint8_range());

cast({int64, Range}, Token) ->
    FullRange = merge_ranges(int64_range(), Range),
    cast_integer(Token, FullRange);
cast(int64, Token) -> cast_integer(Token, int64_range());

cast({int32, Range}, Token) ->
    FullRange = merge_ranges(int32_range(), Range),
    cast_integer(Token, FullRange);
cast(int32, Token) -> cast_integer(Token, int32_range());

cast({int16, Range}, Token) ->
    FullRange = merge_ranges(int16_range(), Range),
    cast_integer(Token, FullRange);
cast(int16, Token) -> cast_integer(Token, int16_range());

cast({int8, Range}, Token) ->
    FullRange = merge_ranges(int8_range(), Range),
    cast_integer(Token, FullRange);
cast(int8, Token) -> cast_integer(Token, int8_range());

cast(string, Token) -> {ok, Token};
cast(boolean, "true") -> {ok, true};
cast(boolean, "false") -> {ok, false};
cast({enum, AllowedVals}, Token) -> cast_enum(Token, AllowedVals);
cast('inet:port-number', Token) -> cast_integer(Token, uint16_range());
cast('inet:ip-address', Token) -> cast_ip_address(Token);
cast({Mod, Type}, Token) when is_atom(Mod) ->
    case Mod:cast_value(Type, Token) of
        {ok, Value} -> {ok, Value};
        {error, _Err} = Err ->
            Err
    end;
cast(Type, _Token) ->
    io:format("MGMTD unsupported leaf type: ~p\n", [Type]),
    {error, {unknown_type, Type}}.

merge_ranges([{min, Min} | Rs], UserRange) ->
    case lists:keyfind(min, 1, UserRange) of
        {min, UMin} when UMin > Min ->
            [{min, UMin} | merge_ranges(Rs, UserRange)];
        _ ->
            [{min, Min} | merge_ranges(Rs, UserRange)]
    end;
merge_ranges([{max, Max} | Rs], UserRange) ->
    case lists:keyfind(max, 1, UserRange) of
        {max, UMax} when UMax < Max ->
            [{max, UMax} | merge_ranges(Rs, UserRange)];
        _ ->
            [{max, Max} | merge_ranges(Rs, UserRange)]
    end;
merge_ranges([], _) ->
    [].

uint64_range() -> [{min, 0}, {max, 18446744073709551615}].
uint32_range() -> [{min, 0}, {max, 4294967295}].
uint16_range() -> [{min, 0}, {max, 65535}].
uint8_range() -> [{min, 0}, {max, 255}].

int64_range() -> [{min, -9223372036854775808}, {max, 9223372036854775807}].
int32_range() -> [{min, -2147483648}, {max, 2147483647}].
int16_range() -> [{min, -32768}, {max, 32767}].
int8_range() -> [{min, -128}, {max, 127}].

cast_integer(Token, Range) ->
    case catch list_to_integer(Token) of
        {'EXIT', _} ->
            {error, "Expected an integer value"};
        Int ->
            cast_integer_in_range(Int, Range)
    end.

cast_integer_in_range(Int, [{min, Min} | Rs]) when Int >= Min ->
    cast_integer_in_range(Int, Rs);
cast_integer_in_range(Int, [{max, Max} | Rs]) when Int =< Max ->
    cast_integer_in_range(Int, Rs);
cast_integer_in_range(Int, []) ->
    {ok, Int};
cast_integer_in_range(_Int, _) ->
    {error, "Value out of range"}.

cast_enum(Token, AllowedVals) ->
    lists:foldl(fun(Enum, Result) ->
                    cast_enum_val(Enum, Token, Result)
              end, {error, "Unkknown enum value"}, AllowedVals).

cast_enum_val(_Enum, _Token, {ok, Res}) -> {ok, Res};
cast_enum_val({Enum, _Desc}, Enum, _) -> {ok, Enum};
cast_enum_val(Enum, Enum, _) -> {ok, Enum};
cast_enum_val(_,_, Res) -> Res.

cast_ip_address(Token) ->
    case inet:parse_address(Token) of
        {ok, _} = Ok -> Ok;
        {error, _Err} -> {error, "Invalid IP Address"}
    end.