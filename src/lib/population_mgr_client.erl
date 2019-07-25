%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Copyright (C) 2018 ApproximateReality
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%----------------------------------------------------------------------------
%%% @doc PopulationMgrClient
%%% @end
%%%----------------------------------------------------------------------------
-module(population_mgr_client).

%% API
-export([
  new_run/0,
  restart_run/0,
  agent_terminated/1,
  set_goal_reached/0,
  set_evaluations/4,
  validation_complete/2,
  set_op_tag/1
]).

%% Xref
-ignore_xref([
  new_run/0,
  restart_run/0,
  agent_terminated/1,
  set_goal_reached/0,
  set_evaluations/4,
  validation_complete/2,
  set_op_tag/1
]).

%%%============================================================================
%%% API
%%%============================================================================

%%-----------------------------------------------------------------------------
%% @doc new_run message.
%% @end
%%-----------------------------------------------------------------------------
-spec new_run() -> ok.
new_run() ->
  population_mgr_sup:start_population_mgr(),
  ok.

%%-----------------------------------------------------------------------------
%% @doc restart_run message.
%% @end
%%-----------------------------------------------------------------------------
-spec restart_run() -> ok.
restart_run() ->
  population_mgr_sup:restart_population_mgr(),
  ok.

%%-----------------------------------------------------------------------------
%% @doc agent_terminated message.
%% @end
%%-----------------------------------------------------------------------------
-spec agent_terminated(models:agent_id()) -> ok.
agent_terminated(AgentId) ->
  population_mgr:agent_terminated(AgentId),
  ok.

%%-----------------------------------------------------------------------------
%% @doc set_goal_reached message.
%% @end
%%-----------------------------------------------------------------------------
-spec set_goal_reached() -> ok.
set_goal_reached() ->
  population_mgr:set_goal_reached(),
  ok.

%%-----------------------------------------------------------------------------
%% @doc set_evaluations message.
%% @end
%%-----------------------------------------------------------------------------
-spec set_evaluations(models:specie_id(), integer(), integer(), integer()) -> ok.
set_evaluations(SpecieId, AEA, CycleAcc, TimeAcc) ->
  population_mgr:set_evaluations(SpecieId, AEA, CycleAcc, TimeAcc),
  ok.

%%-----------------------------------------------------------------------------
%% @doc validation_complete message.
%% @end
%%-----------------------------------------------------------------------------
-spec validation_complete(models:agent_id(), float()) -> ok.
validation_complete(AgentId, Fitness) ->
  population_mgr:validation_complete(AgentId, Fitness),
  ok.

%%-----------------------------------------------------------------------------
%% @doc set_op_tag message.
%% @end
%%-----------------------------------------------------------------------------
-spec set_op_tag(pause | continue) -> ok.
set_op_tag(OpTag) ->
  population_mgr:set_op_tag(OpTag),
  ok.

%%=============================================================================
%% Internal functions
%%=============================================================================