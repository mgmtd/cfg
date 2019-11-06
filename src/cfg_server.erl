%%%-------------------------------------------------------------------
%%% @author Sean Hinde <sean@Seans-MacBook.local>
%%% @copyright (C) 2019, Sean Hinde
%%% @doc Gen server to own schema tables
%%%
%%% @end
%%% Created : 12 Sep 2019 by Sean Hinde <sean@Seans-MacBook.local>
%%%-------------------------------------------------------------------
-module(cfg_server).

-behaviour(gen_server).

-include("cfg.hrl").

%% API
-export([start_link/0, load_schema/2,
         commit/1,
         subscribe/3,
         unsubscribe/1,
         show_subscriptions/0, subscriptions/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3, format_status/2]).

-define(SERVER, ?MODULE).

-record(state, {
                subs = ets:new(subscriptions, []),
                sub_pids = ets:new(sub_pids, [bag]),
                sub_refs = ets:new(sub_refs, [])
               }).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%% @end
%%--------------------------------------------------------------------
-spec start_link() -> {ok, Pid :: pid()} |
                      {error, Error :: {already_started, pid()}} |
                      {error, Error :: term()} |
                      ignore.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

load_schema(NS, ParsedSchema) ->
    gen_server:call(?MODULE, {load_schema, NS, ParsedSchema}).

subscribe(Path, Pid, Opts) when is_pid(Pid); is_atom(Pid); is_list(Opts) ->
    gen_server:call(?SERVER, {subscribe, Path, Pid, Opts}).

unsubscribe(Ref) ->
    gen_server:call(?SERVER, {unsubscribe, Ref}).

show_subscriptions() ->
    Subscriptions = subscriptions(),
    lists:foreach(fun({{Path, Pid,_Ref}, _Opts}) ->
                          io:format("~p => ~p~n",[Pid, Path])
                  end, Subscriptions).

subscriptions() ->
    gen_server:call(?SERVER, get_subscriptions).

commit(Txn) ->
    gen_server:call(?SERVER, {commit, Txn}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%% @end
%%--------------------------------------------------------------------
-spec init(Args :: term()) -> {ok, State :: term()} |
                              {ok, State :: term(), Timeout :: timeout()} |
                              {ok, State :: term(), hibernate} |
                              {stop, Reason :: term()} |
                              ignore.
init([]) ->
    process_flag(trap_exit, true),
    cfg_schema:init(),
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%% @end
%%--------------------------------------------------------------------
-spec handle_call(Request :: term(), From :: {pid(), term()}, State :: term()) ->
                         {reply, Reply :: term(), NewState :: term()} |
                         {reply, Reply :: term(), NewState :: term(), Timeout :: timeout()} |
                         {reply, Reply :: term(), NewState :: term(), hibernate} |
                         {noreply, NewState :: term()} |
                         {noreply, NewState :: term(), Timeout :: timeout()} |
                         {noreply, NewState :: term(), hibernate} |
                         {stop, Reason :: term(), Reply :: term(), NewState :: term()} |
                         {stop, Reason :: term(), NewState :: term()}.
handle_call({load_schema, NS, ParsedSchema}, _From, State) ->
    Reply = cfg_schema:install_schema(NS, ParsedSchema),
    {reply, Reply, State};
handle_call({subscribe, Path, Pid, Opts}, _From, State) ->
    case cfg:lookup(Path) of
        {error, _Reason} = Err ->
            {reply, Err, State};
        {ok, Config} ->
            Ref = erlang:make_ref(),
            MonRef = erlang:monitor(process, Pid),
            ets:insert(State#state.subs, {{Path, Pid, Ref}, Opts}),
            ets:insert(State#state.sub_refs, {Ref, {Path, Pid, MonRef}}),
            ets:insert(State#state.sub_pids, {Pid, {Path, Ref}}),
            Pid ! {updated_config, Ref, Config},
            {reply, {ok, Ref}, State}
    end;
handle_call({unsubscribe, Ref}, _From, State) ->
    case ets:lookup(State#state.sub_refs, Ref) of
        [{Ref, {Path, Pid, MonRef}}] ->
            erlang:demonitor(MonRef),
            ets:delete(State#state.subs, {Path, Pid, Ref}),
            ets:delete(State#state.sub_refs, Ref),
            ets:delete_object(State#state.sub_pids, {Pid, {Path, Ref}}),
            {reply, ok, State};
        [] ->
            {reply, ok, State}
    end;
handle_call(get_subscriptions, _From, State) ->
    Subs = ets:tab2list(State#state.subs),
    {reply, Subs, State};
handle_call({commit, Txn}, _From, State) ->
    cfg_txn:commit(Txn),
    Reply = ok,
    {reply, Reply, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%% @end
%%--------------------------------------------------------------------
-spec handle_cast(Request :: term(), State :: term()) ->
                         {noreply, NewState :: term()} |
                         {noreply, NewState :: term(), Timeout :: timeout()} |
                         {noreply, NewState :: term(), hibernate} |
                         {stop, Reason :: term(), NewState :: term()}.
handle_cast(_Request, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%% @end
%%--------------------------------------------------------------------
-spec handle_info(Info :: timeout() | term(), State :: term()) ->
                         {noreply, NewState :: term()} |
                         {noreply, NewState :: term(), Timeout :: timeout()} |
                         {noreply, NewState :: term(), hibernate} |
                         {stop, Reason :: normal | term(), NewState :: term()}.
handle_info({'DOWN', _, _, Pid, _}, State) ->
    %% Process down, remove all the subscriptions of this process
    case ets:lookup(State#state.sub_pids, Pid) of
        [] ->
            ok;
        Recs ->
            lists:foreach(
              fun({_Pid, {Path, Ref}}) ->
                      [{Ref, {Path, Pid, MonRef}}] =
                          ets:lookup(State#state.sub_refs, Ref),
                      erlang:demonitor(MonRef),
                      ets:delete(State#state.sub_refs, Ref),
                      ets:delete(State#state.subs, {Path, Pid, Ref})
              end, Recs),
            %% Delete all entries from the duplicate_bag with this Pid
            ets:delete(State#state.sub_pids, Pid)
    end,
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%% @end
%%--------------------------------------------------------------------
-spec terminate(Reason :: normal | shutdown | {shutdown, term()} | term(),
                State :: term()) -> any().
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%% @end
%%--------------------------------------------------------------------
-spec code_change(OldVsn :: term() | {down, term()},
                  State :: term(),
                  Extra :: term()) -> {ok, NewState :: term()} |
                                      {error, Reason :: term()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called for changing the form and appearance
%% of gen_server status when it is returned from sys:get_status/1,2
%% or when it appears in termination error logs.
%% @end
%%--------------------------------------------------------------------
-spec format_status(Opt :: normal | terminate,
                    Status :: list()) -> Status :: term().
format_status(_Opt, Status) ->
    Status.

%%%===================================================================
%%% Internal functions
%%%===================================================================
