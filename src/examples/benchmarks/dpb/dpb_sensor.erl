%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%   Copyright (C) 2009 by Gene Sher, DXNN Research Group,
%%%   CorticalComputer@gmail.com
%%%
%%%   The original release of this source code was introduced and explained
%%%   in a book (available for purchase on Amazon) by Gene Sher:
%%%     
%%%     Handbook of Neuroevolution Through Erlang. Springer 2012,
%%%     print ISBN: 978-1-4614-4462-6 ebook ISBN: 978-1-4614-4463-6.
%%% 
%%%   Licensed under the Apache License, Version 2.0 (the "License");
%%%   you may not use this file except in compliance with the License.
%%%   You may obtain a copy of the License at
%%%
%%%     http://www.apache.org/licenses/LICENSE-2.0
%%%
%%%   Unless required by applicable law or agreed to in writing, software
%%%   distributed under the License is distributed on an "AS IS" BASIS,
%%%   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%%   See the License for the specific language governing permissions and
%%%   limitations under the License.
%%%%%%%%%%%%%%%%%%%% Deus Ex Neural Network :: DXNN %%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%   Modified Copyright (C) 2018 ApproximateReality
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%----------------------------------------------------------------------------
%%% @doc Double pole balancing sensor for benchmarking purposes.
%%% @end
%%%----------------------------------------------------------------------------
-module(dpb_sensor).

-behaviour(sensor).

%% API
-export([]).

%% Sensor callbacks
-export([
  init/1,
  sense/2,
  percept/2
]).

%%%============================================================================
%%% Types
%%%============================================================================

-type state() :: {}.

%%%============================================================================
%%% Sensor callbacks
%%%============================================================================

%%%----------------------------------------------------------------------------
%%% @private
%%% @doc Initializes the sensor process.
%%% @end
%%%----------------------------------------------------------------------------
-spec init([]) -> {ok, {}}.
init([]) ->
  {ok, {}}.

%%----------------------------------------------------------------------------
%% @doc sense contacts the pole balancing simulator and requests the
%%      sensory vector.
%% @end
%%----------------------------------------------------------------------------
-spec sense(atom(), {models:agent_id(), integer(), [atom()], pid(), models:sensor_id(),
  models:op_mode(), state()}) -> ok.
sense(dpb, {AgentId, _VL, Params, ScapePid, _SensorId, _OpMode, _ModState}) ->
  private_scape:sense(ScapePid, AgentId, self(), Params),
  ok.

%%----------------------------------------------------------------------------
%% @doc Handles the sensory input.
%% @end
%%----------------------------------------------------------------------------
-spec percept(atom(), {[float()] | destroyed, models:agent_id(), integer(),
  [atom()], state()}) -> {[float()], state()}.
percept(dpb, {SensoryInput, _AgentId, VL, _Params, ModState}) ->
  case length(SensoryInput) of
    VL ->
      {SensoryInput, ModState};
    _ ->
      logr:error({sensor, percept, error, "dpb", [VL, SensoryInput]}),
      {[0.0], ModState}
  end.