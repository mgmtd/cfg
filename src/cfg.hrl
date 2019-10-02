%%%-------------------------------------------------------------------
%%% @author Sean Hinde <sean@Seans-MacBook.local>
%%% @copyright (C) 2019, Sean Hinde
%%% @doc
%%%
%%% @end
%%% Created : 12 Sep 2019 by Sean Hinde <sean@Seans-MacBook.local>
%%%-------------------------------------------------------------------

%% Record used to define config schema, used to drive cli completion,
%% validation, upgrades.
-record(cfg_schema,
        {
         path = [],           % [NodeName] - full path to node
         node_type = container,            % | leaf | list | leaf_list
         name,
         desc,
         type,
         key = {},            % {NodeName1, NodeName2, Nodename3} for lists
         children = [],       % [{M,F}, {M1,F1}, fun()] | {M,F} | fun()
         opts = []
        }).

%% Record we store in the configuration database after validation against the schema.
-record(cfg,
        {
         path,
         name,
         node_type = container,
         value
        }).
