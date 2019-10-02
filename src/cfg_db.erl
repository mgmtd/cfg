%%%-------------------------------------------------------------------
%%% @author Sean Hinde <sean@Seans-MacBook.local>
%%% @copyright (C) 2019, Sean Hinde
%%% @doc Configuration database backend
%%%
%%% @end
%%% Created : 20 Sep 2019 by Sean Hinde <sean@Seans-MacBook.local>
%%%-------------------------------------------------------------------
-module(cfg_db).

-include("cfg.hrl").

-export([init/2, transaction/1, read/1, write/1]).

-export([cfg_list_to_tree/1]).

%%--------------------------------------------------------------------
%% @doc Wrapper functions around the operations towards the chosen storage
%% engine
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @doc Called once at startup to allow the chosen database backend to
%% create tables etc.
%% --------------------------------------------------------------------
-spec init(mnesia | config, proplists:proplist()) -> ok.
init(Backend, Opts) when Backend == mnesia;
                   Backend == config ->
    Backend:init(Opts),
    ets:insert(cfg_meta, {backend, Backend}).

transaction(Fun) when is_function(Fun) ->
    Backend = backend(),
    Backend:transaction(Fun).

read(Key) ->
    Backend = backend(),
    Backend:read(Key).

write(#cfg{} = Cfg) ->
    Backend = backend(),
    Backend:write(Cfg).

backend() ->
    case ets:lookup(cfg_meta, backend) of
        [{_, Backend}] ->
            Backend;
        _ ->
            config
    end.

%%-------------------------------------------------------------------
%% @doc Construct a tree of configuration items from a flat list of
%% #cfg{} records such as might be extracted from the configuration
%% database.
%%
%% Container nodes have their value field set to a list of child nodes
%% @end
%% -------------------------------------------------------------------
-spec cfg_list_to_tree([#cfg{}]) -> [#cfg{}].
cfg_list_to_tree(Cfgs) ->
    cfg_list_to_tree(Cfgs, cfg_zntrees:root(root)).

cfg_list_to_tree([Cfg|Cfgs], Z) ->
    Z1 = zntree_insert_item(Cfg, cfg_zntrees:children(Z)),
    cfg_list_to_tree(Cfgs, Z1);
cfg_list_to_tree([], Z) ->
    %% Finally extract a simple cfg tree from the zntree, skipping root
    zntree_to_cfg_tree(cfg_zntrees:children(Z)).

% Insert an item in the zntree. Z points to the children of the root node
zntree_insert_item(#cfg{path = Path} = Cfg, Z) ->
    Z1 = zntree_node_at_path(Path, Z),
    Z2 = cfg_zntrees:insert(Cfg, Z1),
    %% Z2 now points to our new node. Point it back to the root node ready
    %% for another item to be inserted
    zntree_root(Z2).

zntree_node_at_path([_Path], Z) ->
    %% Z now points to the same level as Path
    Z;
zntree_node_at_path([P|Ps], Z) ->
    Z1 = zntree_search(P, Z),
    Z2 = cfg_zntrees:children(Z1),
    zntree_node_at_path(Ps, Z2).

%% Horizontal search left to right. Node must exist
zntree_search(P, Z) ->
    case cfg_zntrees:value(Z) of
        #cfg{name = P} ->
            Z;
        #cfg{} ->
            zntree_search(P, cfg_zntrees:right(P, Z))
    end.

zntree_root(Z) ->
    Z1 = cfg_zntrees:parent(Z),
    case cfg_zntrees:value(Z1) of
        root ->
            Z1;
        _ ->
            zntree_root(Z1)
    end.

%% Convert the zntree of our cfg records into a tree of #cfg{} records
-spec zntree_to_cfg_tree(cfg_zntrees:zntree()) -> list().
zntree_to_cfg_tree(Zntree) ->
    zntree_to_cfg_tree(Zntree, []).

zntree_to_cfg_tree({_Thread, {_Left, _Right = []}}, Acc) ->
    Acc;
zntree_to_cfg_tree(Z, Acc) ->
    case cfg_zntrees:value(Z) of
        #cfg{node_type = container} = Cfg ->
            Acc1 = [Cfg#cfg{value = zntree_to_cfg_tree(cfg_zntrees:children(Z), [])} | Acc],
            zntree_to_cfg_tree(cfg_zntrees:right(Z), Acc1);
        #cfg{node_type = leaf} = Cfg ->
            zntree_to_cfg_tree(cfg_zntrees:right(Z), [Cfg | Acc]);
        #cfg{node_type = leaf_list} = Cfg ->
            zntree_to_cfg_tree(cfg_zntrees:right(Z), [Cfg | Acc]);
        #cfg{node_type = list} = Cfg ->
            zntree_to_cfg_tree(cfg_zntrees:right(Z), [Cfg | Acc])
    end.
