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
%%% @doc Double pole balancing w/ damping morphology for benchmarking
%%%      purposes.
%%% @end
%%%----------------------------------------------------------------------------
-module(dpb_w_damping).

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
  [
    models:sensor(#{
      id => undefined,
      name => {dpb_sensor, dpb},
      type => standard,
      cx_id => undefined,
      scape => {private, dpb},
      vl => 3,
      fanout_ids => [],
      generation => undefined,
      format => undefined,
      parameters => [3]
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
      name => {dpb_actuator, dpb},
      type => standard,
      cx_id => undefined,
      scape => {private, dpb},
      vl => 1,
      fanin_ids => [],
      generation => undefined,
      format => undefined,
      parameters => [with_damping, 1]
    })
  ].