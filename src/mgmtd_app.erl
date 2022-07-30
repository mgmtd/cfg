%%%-------------------------------------------------------------------
%% @doc mgmtd public API
%% @end
%%%-------------------------------------------------------------------

-module(mgmtd_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    mgmtd_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
