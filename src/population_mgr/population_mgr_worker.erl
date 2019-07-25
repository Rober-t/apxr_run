%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Copyright (C) 2018 ApproximateReality
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%----------------------------------------------------------------------------
%%% @doc The PopulationMgrWorker is responsible for spawning the population_mgr
%%%      process.
%%% @end
%%%----------------------------------------------------------------------------
-module(population_mgr_worker).

-behaviour(gen_server).

%% Start/Stop
-export([
  start_link/0
]).

%% gen_server callbacks
-export([
  init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2
]).

%% Xref
-ignore_xref([
  start_link/0
]).

%%%============================================================================
%%% Types
%%%============================================================================

-record(population_mgr_worker_state, {
  population_mgr_pid :: pid() | undefined
}).

-type state() :: #population_mgr_worker_state{}.

%%%============================================================================
%%% API
%%%============================================================================

%%-----------------------------------------------------------------------------
%% @doc The start_link function spawns the PopulationMgrWorker server.
%% @end
%%-----------------------------------------------------------------------------
-spec start_link() -> {ok, pid()}.
start_link() ->
  gen_server:start_link(?MODULE, [], []).

%%%============================================================================
%%% gen_server callbacks
%%%============================================================================

%%%----------------------------------------------------------------------------
%%% @private
%%% @doc Initializes the PopulationMgrWorker server.
%%% @end
%%%----------------------------------------------------------------------------
-spec init([]) -> {ok, state()}.
init([]) ->
  process_flag(trap_exit, true),
  logr:debug({population_mgr_worker, init, ok, undefined, []}),
  Pid = population_mgr:start(node()),
  State = #population_mgr_worker_state{population_mgr_pid = Pid},
  {ok, State}.

%%-----------------------------------------------------------------------------
%% @private
%% @doc Handle call messages.
%% @end
%%-----------------------------------------------------------------------------
-spec handle_call(term(), {pid(), term()}, state()) -> {reply, ok, state()}.
handle_call(_Request, _From, State) ->
  logr:warning({population_mgr_worker, msg, error, "unexpected handle_call", []}),
  {reply, ok, State}.

%%-----------------------------------------------------------------------------
%% @private
%% @doc Handle cast messages.
%% @end
%%-----------------------------------------------------------------------------
-spec handle_cast(term(), state()) -> {noreply, state()}.
handle_cast(_Request, State) ->
  logr:warning({population_mgr_worker, msg, error, "unexpected handle_cast", []}),
  {noreply, State}.

%%-----------------------------------------------------------------------------
%% @private
%% @doc Handle info messages.
%% @end
%%-----------------------------------------------------------------------------
-spec handle_info(any(), state()) -> {noreply, state()}.
handle_info(Info, State) ->
  case Info of
    {'EXIT', _Pid, normal} ->
      {stop, normal, State};
    {'EXIT', _Pid, shutdown} ->
      {stop, shutdown, State};
    {'EXIT', _Pid, Reason} ->
      {stop, Reason, State};
    UnexpectedMsg ->
      logr:warning({population_mgr_worker, msg, error, "unexpected info message", [UnexpectedMsg]}),
      {noreply, State}
  end.

%%-----------------------------------------------------------------------------
%% @private
%% @doc This function is called by a gen_server when it is about to terminate.
%%      It should be the opposite of Module:init and do any necessary
%%      cleaning up. When it returns, the gen_server terminates with Reason.
%%      The return value is ignored.
%% @end
%%-----------------------------------------------------------------------------
-spec terminate(any(), state()) -> ok.
terminate(Reason, State) ->
  logr:info({population_mgr_worker, status, ok, "population_mgr_worker terminated", [Reason]}),
  Pid = State#population_mgr_worker_state.population_mgr_pid,
  Pid ! stop,
  ok.

%%%============================================================================
%%% Internal functions
%%%============================================================================
