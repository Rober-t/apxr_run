%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Copyright (C) 2018 ApproximateReality
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%----------------------------------------------------------------------------
%%% @doc The scape_mgr is responsible for starting and stopping scapes.
%%%----------------------------------------------------------------------------
-module(scape_mgr).

-behaviour(gen_server).

%% Start/Stop
-export([
  start_link/0
]).

%% API
-export([
  start_scape/5,
  stop_scape/1
]).

%% gen_server callbacks
-export([
  init/1,
  handle_call/3,
  handle_cast/2
]).

%% Xref
-ignore_xref([
  start_link/0,
  start_scape/1
]).

%%%============================================================================
%%% Types
%%%============================================================================

-record(scape_mgr_state, {}).

-type state() :: #scape_mgr_state{}.

%%%============================================================================
%%% API
%%%============================================================================

%%-----------------------------------------------------------------------------
%% @doc The gen_server:start_link function starts up the scape_mgr server.
%% @end
%%-----------------------------------------------------------------------------
-spec start_link() -> {error, string()} | {ok, pid()}.
start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%-----------------------------------------------------------------------------
%% @doc The start_scape function starts a new scape.
%% @end
%%-----------------------------------------------------------------------------
-spec start_scape(float(), float(), float(), float(), atom()) -> ok.
start_scape(X, Y, Width, Height, ModName) ->
  gen_server:cast(?MODULE, {start_scape, X, Y, Width, Height, ModName}).

%%-----------------------------------------------------------------------------
%% @doc The stop_scape function stops a scape.
%% @end
%%-----------------------------------------------------------------------------
-spec stop_scape(atom()) -> ok.
stop_scape(ModName) ->
  gen_server:cast(?MODULE, {stop_scape, ModName}).

%%%============================================================================
%%% gen_server callbacks
%%%============================================================================

%%-----------------------------------------------------------------------------
%% @private
%% @doc Initializes the ScapeMgr server.
%% @end
%%-----------------------------------------------------------------------------
-spec init([]) -> {ok, state()}.
init([]) ->
  application:ensure_all_started(shards),
  utils:random_seed(),
  logr:debug({scape_mgr, init, ok, undefined, []}),
  InitState = #scape_mgr_state{},
  {ok, InitState}.

%%-----------------------------------------------------------------------------
%% @private
%% @doc Handle call messages.
%% @end
%%-----------------------------------------------------------------------------
-spec handle_call(term(), {pid(), term()}, state()) -> {reply, ok, state()}.
handle_call(Request, From, State) ->
  logr:warning({scape_mgr, msg, error, "unexpected handle_call", [Request, From]}),
  {reply, ok, State}.

%%-----------------------------------------------------------------------------
%% @private
%% @doc Handle cast messages.
%% @end
%%-----------------------------------------------------------------------------
-spec handle_cast(
  {start_scape, float(), float(), float(), float(), atom()} |
  {stop_scape, atom()}, state()) -> {noreply, state()} | {stop, normal | shutdown, state()}.
handle_cast({start_scape, X, Y, Width, Height, ModName}, State) when is_atom(ModName) ->
  shards:new(t1, [set, public, named_table, {write_concurrency, true}, {read_concurrency, true}]),
  shards:new(t2, [set, public, named_table, {write_concurrency, true}, {read_concurrency, true}]),
  shards:new(t3, [set, public, named_table, {write_concurrency, true}, {read_concurrency, true}]),
  shards:new(t4, [set, public, named_table, {write_concurrency, true}, {read_concurrency, true}]),
  shards:new(t5, [set, public, named_table, {write_concurrency, true}, {read_concurrency, true}]),
  shards:new(t6, [set, public, named_table, {write_concurrency, true}, {read_concurrency, true}]),
  shards:new(t7, [set, public, named_table, {write_concurrency, true}, {read_concurrency, true}]),
  shards:new(t8, [set, public, named_table, {write_concurrency, true}, {read_concurrency, true}]),
  shards:new(t9, [set, public, named_table, {write_concurrency, true}, {read_concurrency, true}]),
  shards:new(t10, [set, public, named_table, {write_concurrency, true}, {read_concurrency, true}]),
  {ok, ScapePid} = scape_sup:start_scape(X, Y, Width, Height, ModName),
  true = ets:insert(scape_names_pids, {ModName, ScapePid}),
  {noreply, State};

handle_cast({stop_scape, ModName}, State) when is_atom(ModName) ->
  [{ModName, ScapePid}] = ets:lookup(scape_names_pids, ModName),
  ok = scape_sup:stop_scape(ScapePid),
  {noreply, State}.

%%%============================================================================
%%% Internal functions
%%%============================================================================