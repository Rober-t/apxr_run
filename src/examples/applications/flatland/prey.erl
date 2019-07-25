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
%%% @doc Prey morphology for the Flatland example application.
%%% @end
%%%----------------------------------------------------------------------------
-module(prey).

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
  Pi = math:pi(),
  [
    models:sensor(#{
      id => undefined,
      name => {flatland_sensor, distance_scanner},
      type => standard,
      cx_id => undefined,
      scape => {public, flatland},
      vl => 5,
      fanout_ids => [],
      generation => undefined,
      format => no_geo,
      parameters => [[Pi/2], [5], [Pi*0/2]]
    }),
    models:sensor(#{
      id => undefined,
      name => {flatland_sensor, color_scanner},
      type => standard,
      cx_id => undefined,
      scape => {public, flatland},
      vl => 5,
      fanout_ids => [],
      generation => undefined,
      format => no_geo,
      parameters => [[Pi/2], [5], [Pi*0/2]]
    })
  ].

%%-----------------------------------------------------------------------------
%% @private
%% @doc Returns the list of actuators for this morphology.
%% @end
%%-----------------------------------------------------------------------------
-spec actuators() -> [models:actuator()].
actuators() ->
  [
    models:actuator(#{
      id => undefined,
      name => {flatland_actuator, two_wheels},
      type => standard,
      cx_id => undefined,
      scape => {public, flatland},
      vl => 2,
      fanin_ids => [],
      generation => undefined,
      format => no_geo,
      parameters => [2]
    })
  ].