%%%-------------------------------------------------------------------
%%% @author Sean Hinde <sean@Seans-MacBook.local>
%%% @copyright (C) 2019, Sean Hinde
%%% @doc
%%%
%%% @end
%%% Created : 12 Sep 2019 by Sean Hinde <sean@Seans-MacBook.local>
%%%-------------------------------------------------------------------

-record(tree,
        {
         node_type = container,            % | leaf | list | leaf_list
         name,
         desc,
         type,
         key = {},            % {NodeName1, NodeName2, Nodename3} for lists
         children = [],       % [{M,F}, {M1,F1}, fun()] | {M,F} | fun()
         path = [],           % [NodeName] - full path to node
         opts = []
        }).
