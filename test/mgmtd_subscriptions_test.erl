%%%-------------------------------------------------------------------
%%% @author Sean Hinde <sean@Seans-MacBook.local>
%%% @copyright (C) 2019, Sean Hinde
%%% @doc Eunit tests for cfg_db
%%%
%%% @end
%%% Created : 7 Nov 2019 by Sean Hinde <sean@Seans-MacBook.local>
%%%-------------------------------------------------------------------
-module(mgmtd_subscriptions_test).

-include("../src/mgmtd_schema.hrl").
-include("../include/mgmtd.hrl").

-include_lib("eunit/include/eunit.hrl").


cfg_schema() ->
    [#container{name = "interface",
                desc = "Interface configuration",
                children = fun() -> interface_schema() end},
     #container{name = "server",
                desc = "Server configuration",
                children = fun() -> server_list_schema() end},
     #container{name = "client",
                desc = "Client configuration",
                children = fun() -> client_list_schema() end}].

interface_schema() ->
    [#leaf{name = "speed",
           desc = "Interface speed",
           type = {enum, [{"1GbE", "1 Gigabit/s Ethernet"}]},
           default = "1GbE"}].

server_list_schema() ->
    [#list{name = "servers",
           desc = "Server list",
           key_names = ["name"],
           data_callback = mgmtd,
           children = fun() -> server_schema() end}].

server_schema() ->
    [#leaf{name = "name",
           desc = "Server name",
           type = string},
     #leaf{name = "host",
           desc = "Server hostname",
           type = 'inet:ip-address',
           default = "127.0.0.1"},
     #leaf{name = "port",
           desc = "Listen port",
           type = 'inet:port-number',
           default = 80}].

client_list_schema() ->
    [#list{name = "clients",
           desc = "Client list",
           key_names = ["host", "port"],
           children = fun() -> client_schema() end}].

client_schema() ->
    [#leaf{name = "name",
           desc = "Server name",
           type = string},
     #leaf{name = "host",
           desc = "Server hostname",
           type = 'inet:ip-address',
           default = "127.0.0.1"},
     #leaf{name = "port",
           desc = "Server port",
           type = 'inet:port-number',
           default = 8080}].


setup_schema() ->
    mgmtd_sup:start_link(),
    %% ok = mgmtd_cfg_db:remove_db("test_db", mnesia),

    mgmtd:load_function_schema(fun() -> cfg_schema() end),
    mgmtd_cfg_db:init("test_db", [{backend, mnesia}]),
    ok.

init_cfg() ->
    ok.

destroy_schema() ->
    mgmtd_schema:remove_schema(),
    mgmtd_cfg_db:remove_db("test_db", mnesia),
    ok.

subscription_test_() ->
    {setup,
     fun() ->
             setup_schema(),
             init_cfg(),
             start_subscriber()
     end,
     fun(Pid) ->
            destroy_schema(),
             exit(Pid, kill)
     end,
     fun(_Pid) ->
             %% Expect the initial value on subscription
             receive
                 {updated, Updated} ->
                     ?_assertEqual([], Updated)
             after 200 ->
                     exit(init_subscription_not_received)
             end,
             %% Add a list item entry. We should expect a subscription message
             %% for the new list item but nothing more
             SetPath = ["server", "servers", {"newlistitem"}, "port", "81"],
             {ok, SchemaPath} = mgmtd_schema:lookup_path(SetPath),
             Txn = mgmtd:txn_new(),
             {ok, Txn2} = mgmtd:txn_set(Txn, SchemaPath),
             ok = mgmtd:txn_commit(Txn2),
             %% Expect the new list item
             receive
                 {updated, Updated1} ->
                     ?_assertEqual([{"newlistitem"}], Updated1)
             after 200 ->
                     exit(subscription_not_received)
             end,
             UpdPath = ["server", "servers", {"newlistitem"}, "port", "8080"],
             {ok, SchemaPath1} = mgmtd_schema:lookup_path(UpdPath),
             Txn3 = mgmtd:txn_new(),
             {ok, Txn4} = mgmtd:txn_set(Txn3, SchemaPath1),
             ok = mgmtd:txn_commit(Txn4),
             %% Expect the intial value on susbcribing to the new list entry
             receive
                 {updated, Updated2} ->
                     ?_assertEqual([{"name","newlistitem"},{"port",["80"]}],
                                   Updated2)
             after 200 ->
                     exit(subscription_not_received)
             end,
             %% And now the updated port number 8080
             receive
                 {updated, Updated3} ->
                     ?_assertEqual([{"host","127.0.0.1"}, {"port",8080}],
                                   Updated3)
             after 200 ->
                     exit(subscription_not_received)
             end
     end}.

start_subscriber() ->
    Self = self(),
    spawn_link(fun() ->
                       {ok, Ref} = mgmtd:subscribe(["server", "servers"], self()),
                       %% We get the initial config (empty list) on subscribing
                       receive
                           {updated_config, Ref, Updated} ->
                               Self ! {updated, Updated}
                       end,
                       %% Now wait for the update that added the newlist entry
                       receive
                           {updated_config, Ref, Updated2} ->
                               Self ! {updated, Updated2}
                       end,
                       {ok, Ref2} = mgmtd:subscribe(["server", "servers", {"newlistitem"}], self()),
                       %% Wait for the initial value from the second subscription
                       receive
                           {updated_config, Ref2, Updated3} ->
                               Self ! {updated, Updated3}
                       end,
                       %% Wait for the updated value after the txn setting port to 8080
                       receive
                           {updated_config, Ref2, Updated5} ->
                               Self ! {updated, Updated5}
                       end
               end).
