%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Copyright (C) 2018 ApproximateReality
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%----------------------------------------------------------------------------
%%% @doc The AgentMgr is responsible for managing agents.
%%%----------------------------------------------------------------------------
-module(agent_mgr).

-behaviour(gen_server).

%% Start/Stop
-export([
  start_link/0
]).

%% API
-export([
  start_agent/2,
  stop_agent/1
]).

%% gen_server callbacks
-export([
  init/1,
  handle_call/3,
  handle_cast/2
]).

%% Xref
-ignore_xref([
  start_link/0
]).

%%%============================================================================
%%% Types
%%%============================================================================

-record(agent_mgr_state, {}).

-type state() :: #agent_mgr_state{}.

%%%============================================================================
%%% API
%%%============================================================================

%%-----------------------------------------------------------------------------
%% @doc Starts the AgentMgr server.
%% @end
%%-----------------------------------------------------------------------------
-spec start_link() -> {error, string()} | {ok, pid()}.
start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%-----------------------------------------------------------------------------
%% @doc The start_agent function starts a new AgentWorker.
%% @end
%%-----------------------------------------------------------------------------
-spec start_agent(models:agent_id(), models:op_mode()) -> ok.
start_agent(AgentId, OpMode) ->
  gen_server:cast(?MODULE, {start_agent, AgentId, OpMode}).

%%-----------------------------------------------------------------------------
%% @doc The stop_agent function stops an AgentWorker.
%% @end
%%-----------------------------------------------------------------------------
-spec stop_agent(models:agent_id()) -> ok.
stop_agent(AgentId) when is_tuple(AgentId) ->
  gen_server:cast(?MODULE, {stop_agent, AgentId}).

%%%============================================================================
%%% gen_server callbacks
%%%============================================================================

%%-----------------------------------------------------------------------------
%% @private
%% @doc Initializes the AgentMgr server.
%% @end
%%-----------------------------------------------------------------------------
-spec init([]) -> {ok, state()}.
init([]) ->
  utils:random_seed(),
  logr:debug({agent_mgr, init, ok, undefined, []}),
  InitState = #agent_mgr_state{},
  {ok, InitState}.

%%-----------------------------------------------------------------------------
%% @private
%% @doc Handle call messages.
%% @end
%%-----------------------------------------------------------------------------
-spec handle_call(term(), {pid(), term()}, state()) -> {reply, ok, state()}.
handle_call(Request, From, State) ->
  logr:warning({agent_mgr, msg, error, "unexpected handle_call", [Request, From]}),
  {reply, ok, State}.

%%-----------------------------------------------------------------------------
%% @private
%% @doc Handle cast messages.
%% @end
%%-----------------------------------------------------------------------------
-spec handle_cast(any(), state()) -> {noreply, state()}.
handle_cast({start_agent, AgentId, OpMode}, State) ->
  {ok, AgentPid} = agent_sup:start_agent(AgentId, OpMode),
  ets:insert(agent_ids_pids, {AgentId, AgentPid}),
  {noreply, State};

handle_cast({stop_agent, AgentId}, State) when is_tuple(AgentId) ->
  [{AgentId, AgentPid}] = ets:lookup(agent_ids_pids, AgentId),
  agent_sup:stop_agent(AgentPid),
  {noreply, State}.

%%%============================================================================
%%% Internal functions
%%%============================================================================