%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Copyright (C) 2018 ApproximateReality
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%----------------------------------------------------------------------------
%%% @doc The AgentWorker is responsible for spawning the Exoself process
%%%      (genotype) which in turn spawns the Cortex, Sensors, Neurons, Actuator
%%%      (phenotype) and maybe the private scape.
%%% @end
%%%----------------------------------------------------------------------------
-module(agent_worker).

-behaviour(gen_server).

%% Start/Stop
-export([
  start_link/2
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
  start_link/2
]).

%%%============================================================================
%%% Types
%%%============================================================================

-record(agent_worker_state, {
  exoself_pid :: pid() | undefined,
  agent_id :: models:agent_id(),
  op_mode :: models:op_mode()
}).

-type state() :: #agent_worker_state{}.

%%%============================================================================
%%% API
%%%============================================================================

%%-----------------------------------------------------------------------------
%% @doc The start_link function spawns the AgentWorker server.
%% @end
%%-----------------------------------------------------------------------------
-spec start_link(models:agent_id(), models:op_mode()) -> {ok, pid()}.
start_link(AgentId, OpMode) ->
  gen_server:start_link(?MODULE, {AgentId, OpMode}, []).

%%%============================================================================
%%% gen_server callbacks
%%%============================================================================

%%%----------------------------------------------------------------------------
%%% @private
%%% @doc Initializes the AgentWorker server.
%%% @end
%%%----------------------------------------------------------------------------
-spec init({models:agent_id(), models:op_mode()}) -> {ok, state()}.
init({AgentId, OpMode}) ->
  process_flag(trap_exit, true),
  logr:debug({agent_worker, init, ok, undefined, [AgentId]}),
  gen_server:cast(self(), init_phase2),
  State = #agent_worker_state{agent_id = AgentId, op_mode = OpMode},
  {ok, State}.

%%-----------------------------------------------------------------------------
%% @private
%% @doc Handle call messages.
%% @end
%%-----------------------------------------------------------------------------
-spec handle_call(term(), {pid(), term()}, state()) -> {reply, ok, state()}.
handle_call(_Request, _From, State) ->
  logr:warning({agent_worker, msg, error, "unexpected handle_call", []}),
  {reply, ok, State}.

%%-----------------------------------------------------------------------------
%% @private
%% @doc Handle cast messages.
%% @end
%%-----------------------------------------------------------------------------
-spec handle_cast(init_phase2, state()) -> {noreply, state()}.
handle_cast(init_phase2, State) ->
  NewState = start_exoself(State),
  {noreply, NewState}.

%%-----------------------------------------------------------------------------
%% @private
%% @doc Handle info messages.
%% @end
%%-----------------------------------------------------------------------------
-spec handle_info(any(), state()) -> {noreply, state()}.
handle_info(Info, State) ->
  case Info of
    {'EXIT', _Pid, normal} ->
      {noreply, State};
    {'EXIT', Pid, shutdown} ->
      logr:debug({agent_worker, msg, ok, "shutdown message", [Pid]}),
      {stop, shutdown, State};
    {'EXIT', Pid, terminate_agent} ->
      logr:debug({agent_worker, msg, ok, "terminate_agent message", [Pid]}),
      {stop, normal, State};
    {'EXIT', Pid, Reason} ->
      logr:warning({agent_worker, msg, ok, "exit message", [Pid, Reason]}),
      {stop, Reason, State};
    UnexpectedMsg ->
      logr:warning({agent_worker, msg, error, "unexpected info message", [UnexpectedMsg]}),
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
  case ets:whereis(agent_ids_pids) of
    undefined ->
      ok;
    _Tid ->
      logr:debug({agent_worker, terminate, ok, undefined, [Reason]}),
      AgentId = State#agent_worker_state.agent_id,
      case ets:member(agent_ids_pids, AgentId) of
        true ->
          ets:delete(agent_ids_pids, AgentId);
        false ->
          ok
      end,
      scape_mgr_client:leave(AgentId, [])
  end.

%%%============================================================================
%%% Internal functions
%%%============================================================================

start_exoself(S) ->
  Pid = exoself:start(node()),
  ok = exoself:init_phase2(Pid, S#agent_worker_state.agent_id, S#agent_worker_state.op_mode),
  S#agent_worker_state{exoself_pid = Pid}.