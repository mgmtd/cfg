%%%-------------------------------------------------------------------
%%% @author Sean Hinde <sean@Seans-MacBook.local>
%%% @copyright (C) 2019, Sean Hinde
%%% @doc Eunit tests for cfg_db
%%%
%%% @end
%%% Created : 7 Nov 2019 by Sean Hinde <sean@Seans-MacBook.local>
%%%-------------------------------------------------------------------
-module(cfg_subscriptions_test).

-include("../src/cfg.hrl").

-include_lib("eunit/include/eunit.hrl").


cfg_schema() ->
    [
     cfg:container("interface", "Interface configuration", fun() -> interface_schema() end),
     cfg:container("server", "Server configuration",
                   fun() -> server_list_schema() end),
     cfg:container("client", "Client configuration",
                   fun() -> client_list_schema() end)
    ].

interface_schema() ->
    [
     cfg:leaf( "speed", "Interface speed", {enum, [{"1GbE", "1 Gigabit/s Ethernet"}]}, "1GbE")
    ].

server_list_schema() ->
    [
     cfg:list("servers", "Server list", ["name"],
              fun() -> server_schema() end, [])
    ].

server_schema() ->
   [
    cfg:leaf( "name", "Server name", string, ""),
    cfg:leaf( "host", "Server hostname", ip_addr, "127.0.0.1"),
    cfg:leaf( "port", "Listen port", inet_port, 80)
   ].

client_list_schema() ->
    [
     cfg:list("clients", "Client list", ["host", "port"],
              fun() -> client_schema() end, [])
    ].

client_schema() ->
   [
    cfg:leaf( "name", "Server name", string, ""),
    cfg:leaf( "host", "Server hostname", ip_addr, "127.0.0.1"),
    cfg:leaf( "port", "Server port", inet_port, 8080)
   ].


setup_schema() ->
    cfg_sup:start_link(),
    cfg:init(mnesia, [{db_path, "test_db"}]),
    cfg:load_schema(fun() -> cfg_schema() end),
    ok.

init_cfg() ->
    ok.

destroy_schema() ->
    cfg_server:remove_schema(default),
    cfg:remove_db(mnesia, [{db_path, "test_db"}]),
    ok.

subscription_test_() ->
    {setup,
     fun() ->
             setup_schema(),
             init_cfg(),
             start_subscriber()
     end,
     fun(Pid) ->
             exit(Pid, kill),
             destroy_schema()
     end,
     fun(_Pid) ->
             %% Expect the initial value on subscription
             receive
                 {updated, Updated} ->
                     ?_assertEqual([], Updated)
             after 200 ->
                     exit(init_subscription_not_received)
             end,
             SetPath = ["server", "servers", {"newlistitem"}, "port", "80"],
             {ok, SchemaPath, Value} = cfg_schema:lookup_path(SetPath),
             Txn = cfg:transaction(),
             {ok, Txn2} = cfg:set(Txn, SchemaPath, Value),
             ok = cfg:commit(Txn2),
             %% Expect the updated value
             receive
                 {updated, Updated1} ->
                     ?_assertEqual([{"newlistitem"}], Updated1)
             after 200 ->
                     exit(subscription_not_received)
             end,
             UpdPath = ["server", "servers", {"newlistitem"}, "port", "8080"],
             {ok, SchemaPath1, Value1} = cfg_schema:lookup_path(UpdPath),
             Txn3 = cfg:transaction(),
             {ok, Txn4} = cfg:set(Txn3, SchemaPath1, Value1),
             ok = cfg:commit(Txn4),
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
                     ?_assertEqual([{"name","newlistitem"},{"port",["80"]}],
                                   Updated3)
             after 200 ->
                     exit(subscription_not_received)
             end
     end}.

start_subscriber() ->
    Self = self(),
    spawn_link(fun() ->
                       {ok, Ref} = cfg:subscribe(["server", "servers"], self()),
                       %% We get the initial config on subscribing
                       receive
                           {updated_config, Ref, Updated} =Msg ->
                               ?debugFmt("Got Message ~p~n",[Msg]),
                               Self ! {updated, Updated}
                       end,
                       %% Now wait for the update
                       receive
                           {updated_config, Ref, Updated2} =Msg2 ->
                               ?debugFmt("Got Message2 ~p~n",[Msg2]),
                               Self ! {updated, Updated2}
                       end,
                       {ok, Ref2} = cfg:subscribe(["server", "servers", {"newlistitem"}], self()),
                       receive
                           {updated_config, Ref2, Updated3} =Msg3 ->
                               ?debugFmt("Got leaf Message ~p~n",[Msg3]),
                               Self ! {updated, Updated3}
                       end,
                       %% Now wait for the update
                       receive
                           {updated_config, Ref2, Updated4} =Msg4 ->
                               ?debugFmt("Got leaf Message2 ~p~n",[Msg4]),
                               Self ! {updated, Updated4}
                       end
               end).
