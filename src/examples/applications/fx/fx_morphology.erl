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
%%% @doc FX morphology.
%%% @end
%%%----------------------------------------------------------------------------
-module(fx_morphology).

-behaviour(morphology).

%% API
-export([]).

%% Morphology callbacks
-export([
  sensors/0,
  actuators/0
]).

%%%============================================================================
%%% Morphology callbacks
%%%============================================================================

%%-----------------------------------------------------------------------------
%% @private
%% @doc Returns the list of sensors for this morphology.
%% @end
%%-----------------------------------------------------------------------------
-spec sensors() -> [models:sensor()].
sensors() -> 
  PLISensors = [
    models:sensor(#{
      id => undefined,
      name => {fx_sensor, pli},
      type => standard,
      cx_id => undefined,
      scape => {private, fx},
      vl => HRes,
      fanout_ids => [],
      generation => undefined,
      format => no_geo,
      parameters => [HRes, close]
    }) || HRes <- [20]],
  PCISensors = [
   models:sensor(#{
     id => undefined,
     name => {fx_sensor, pci},
     type => standard,
     cx_id => undefined,
     scape => {private, fx},
     vl => HRes * VRes,
     fanout_ids => [],
     generation => undefined,
     format => {symmetric, [HRes, VRes]},
     parameters => [HRes, VRes]
   }) || HRes <- [20], VRes <- [20]],
  InternalSensors = [
   models:sensor(#{
     id => undefined,
     name => {fx_sensor, internals},
     type => standard,
     cx_id => undefined,
     scape => {private, fx},
     vl => 3,
     fanout_ids => [],
     generation => undefined,
     format => no_geo,
     parameters => [3]
   })],
  PLISensors ++ PCISensors ++ InternalSensors.

%%-----------------------------------------------------------------------------
%% @private
%% @doc Returns the list of actuators that for this morphology.
%% @end
%%-----------------------------------------------------------------------------
-spec actuators() -> [models:actuator()].
actuators() ->
  [
    models:actuator(#{
      id => undefined,
      name => {fx_actuator, trade},
      type => standard,
      cx_id => undefined,
      scape => {private, fx},
      vl => 1,
      fanout_ids => [],
      generation => undefined,
      format => no_geo,
      parameters => []
    })
  ].