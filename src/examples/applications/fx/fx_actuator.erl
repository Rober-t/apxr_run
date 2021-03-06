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
%%% @doc FX actuator.
%%% @end
%%%----------------------------------------------------------------------------
-module(fx_actuator).

-behaviour(actuator).

%% API
-export([]).

%% Actuator callbacks
-export([
  init/1,
  actuate/2
]).

%%%============================================================================
%%% Types
%%%============================================================================

-type state() :: {}.

%%%============================================================================
%%% Actuator callbacks
%%%============================================================================

%%%----------------------------------------------------------------------------
%%% @private
%%% @doc Initializes the actuator process.
%%% @end
%%%----------------------------------------------------------------------------
-spec init([]) -> {ok, {}}.
init([]) ->
  {ok, {}}.

%%-----------------------------------------------------------------------------
%% @doc The actuate function simply forwards the Output vector to the
%%      FX simulator, and waits for the resulting Fitness and
%%      EndFlag from the simulation process.
%% @end
%%-----------------------------------------------------------------------------
-spec actuate(atom(), {models:agent_id(), [float()], [float()], non_neg_integer(),
  pid(), models:actuator_id(), state()}) -> {[float()], atom() | float(), state()}.
actuate(trade, {AgentId, [TradeSignal], _Params, _VL, ScapePid, _ActuatorId, ModState}) ->
  private_scape:actuate(ScapePid, AgentId, self(), trade,
    {'EURUSD15', functions:trinary(TradeSignal)}),
  ModState.