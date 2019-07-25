%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Copyright (C) 2018 ApproximateReality
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%----------------------------------------------------------------------------
%%% @doc ScapeMgrClient
%%% @end
%%%----------------------------------------------------------------------------
-module(scape_mgr_client).

%% API
-export([
  start_scape/5,
  stop_scape/1,
  enter/2,
  sense/3,
  actuate/4,
  leave/2
]).

%% Xref
-ignore_xref([
  start_scape/5,
  stop_scape/1,
  enter/2,
  sense/3,
  actuate/4,
  leave/2
]).

%%%============================================================================
%%% API
%%%============================================================================

%%-----------------------------------------------------------------------------
%% @doc start_scape message.
%% @end
%%-----------------------------------------------------------------------------
-spec start_scape(float(), float(), float(), float(), atom()) -> ok.
start_scape(X, Y, Width, Height, ModName) ->
  scape_mgr:start_scape(X, Y, Width, Height, ModName),
  ok.

%%-----------------------------------------------------------------------------
%% @doc stop_scape message.
%% @end
%%-----------------------------------------------------------------------------
-spec stop_scape(atom()) -> ok.
stop_scape(ModName) ->
  scape_mgr:stop_scape(ModName),
  ok.

%%-----------------------------------------------------------------------------
%% @doc enter message.
%% @end
%%-----------------------------------------------------------------------------
-spec enter(models:agent_id(), [any()]) -> ok.
enter(AgentId, Params) ->
  scape:enter(AgentId, Params),
  ok.

%%-----------------------------------------------------------------------------
%% @doc sense message.
%% @end
%%-----------------------------------------------------------------------------
-spec sense(models:agent_id(), pid(), any()) -> ok.
sense(AgentId, SensorPid, Params) ->
  scape:sense(AgentId, SensorPid, Params),
  ok.

%%-----------------------------------------------------------------------------
%% @doc actuate message.
%% @end
%%-----------------------------------------------------------------------------
-spec actuate(models:agent_id(), pid(), atom(), any()) -> ok.
actuate(AgentId, ActuatorPid, Function, Params) ->
  scape:actuate(AgentId, ActuatorPid, Function, Params),
  ok.

%%-----------------------------------------------------------------------------
%% @doc leave message.
%% @end
%%-----------------------------------------------------------------------------
-spec leave(models:agent_id(), [any()]) -> ok.
leave(AgentId, Params) ->
  scape:leave(AgentId, Params),
  ok.

%%=============================================================================
%% Internal functions
%%=============================================================================