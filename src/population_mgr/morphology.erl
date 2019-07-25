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
%%% @doc Defines generic morphology behavior.
%%%      The list of morphologies defines the list of sensors and actuators
%%%      available to the NNs in a population. Since the morphology defines
%%%      the sensors and actuators of the NN system, this list effectively
%%%      defines the problem or simulation to which the evolving population of
%%%      NN systems will be applied, and for what purpose the agents will be
%%%      evolved. The sensors/actuators/scape are a separate part from the NN
%%%      itself, all specified through the morphology module.
%%% @end
%%%----------------------------------------------------------------------------
-module(morphology).

%% API
-export([
  get_init_sensors/1, % feature selection
  get_init_actuators/1, % feature selection
  get_sensors/1,
  get_actuators/1,
  get_init_substrate_cpps/2,
  get_init_substrate_ceps/2,
  get_substrate_cpps/2,
  get_substrate_ceps/2
]).

%% Xref
-ignore_xref([
  behaviour_info/1
]).

%%%============================================================================
%%% Behavior
%%%============================================================================

-callback sensors() -> [sensor:sensor()].
-callback actuators() -> [actuator:actuator()].

%%%============================================================================
%%% API
%%%============================================================================

%%-----------------------------------------------------------------------------
%% @doc The get_init_sensors starts the population off with the NN based
%%      agents using just a single sensor, exploring other available sensors
%%      within the morphology as it evolves.
%% @end
%%-----------------------------------------------------------------------------
-spec get_init_sensors(atom()) -> [sensor:sensor()].
get_init_sensors(Mod) ->
  M = utils:get_module(Mod),
  Sensors = M:sensors(),
  [lists:nth(1, Sensors)].

%%-----------------------------------------------------------------------------
%% @doc The get_init_actuators starts the population off with the NN based
%%      agents using just a single actuator, exploring other available
%%      actuators within the morphology as it evolves.
%% @end
%%-----------------------------------------------------------------------------
-spec get_init_actuators(atom()) -> [actuator:actuator()].
get_init_actuators(Mod) ->
  M = utils:get_module(Mod),
  Actuators = M:actuators(),
  [lists:nth(1, Actuators)].

%%-----------------------------------------------------------------------------
%% @doc The get_init_sensors starts the population off with the NN based
%%      agents using all available sensors from the start.
%% @end
%%-----------------------------------------------------------------------------
-spec get_sensors(atom()) -> [sensor:sensor()].
get_sensors(Mod) ->
  M = utils:get_module(Mod),
  M:sensors().

%%-----------------------------------------------------------------------------
%% @doc The get_init_actuators starts the population off with the NN based
%%      agents using all available actuators from the start.
%% @end
%%-----------------------------------------------------------------------------
-spec get_actuators(atom()) -> [actuator:actuator()].
get_actuators(Mod) ->
  M = utils:get_module(Mod),
  M:actuators().

%%-----------------------------------------------------------------------------
%% @doc The get_init_substrate_cpps starts the population off with the NN based
%%      agents using just a single substrate_cpp, exploring other available
%%      substrate_cpps within the morphology as it evolves.
%% @end
%%-----------------------------------------------------------------------------
-spec get_init_substrate_cpps(integer(), abcn | iterative | none)
-> [models:sensor()].
get_init_substrate_cpps(Dimensions, Plasticity) ->
  SubstrateCPPs = get_substrate_cpps(Dimensions, Plasticity),
  [lists:nth(1, SubstrateCPPs)].

%%-----------------------------------------------------------------------------
%% @doc The get_init_substrate_ceps starts the population off with the NN based
%%      agents using just a single substrate_cep, exploring other available
%%      substrate_ceps within the morphology as it evolves.
%% @end
%%-----------------------------------------------------------------------------
-spec get_init_substrate_ceps(integer(), abcn | iterative | none)
-> [models:actuator()].
get_init_substrate_ceps(Dimensions, Plasticity) ->
  SubstrateCEPs = get_substrate_ceps(Dimensions, Plasticity),
  [lists:nth(1, SubstrateCEPs)].

%%-----------------------------------------------------------------------------
%% @doc The get_substrate_cpps starts the population off with the NN based
%%      agents using substrate_cpps determined by Dimensions and Plasticity.
%%      Substrate CPPs:
%%      x cartesian: The cartesian cpp simply forwards to the NN the appended
%%        coordinates of the two connected neurodes. Because each neurode has
%%        a coordinate specified by a list of length: Dimension, the vector
%%        specifying the two appended coordinates will have
%%        vl = Dimensions * 2. For example: [X1,Y1,Z1,X2,Y2,Z2] will have a
%%        vector length of dimension: vl = 3*2 = 6.
%%      x centripetal_distances: This cpp uses the Cartesian coordinates of
%%        the two neurodes to calculate the Cartesian distance of neurode_1
%%        to the center of the substrate located at the origin, and the
%%        Cartesian distance of neurode_2 to the center of the substrate.
%%        It then fans out to the NN the vector of length 2, composed of the
%%        two distances.
%%      x cartesian_distance: This cpp calculates the Cartesian distance
%%        between the two neurodes, forwarding the resulting vector of
%%        length 1 to the NN.
%%      x cartesian_CoordDiffs: This cpp calculates the difference between
%%        each coordinate element of the two neurodes, and thus for this cpp,
%%        the vl = Dimensions.
%%      x cartesian_GaussedCoordDiffs: Exactly the same as the above cpp, but
%%        each of the values is first sent through the Gaussian function
%%        before it is entered into the vector.
%%      x polar: This cpp converts the Cartesian coordinates to polar
%%        coordinates. This can only be done if the substrate is 2d.
%%      x spherical: This cpp converts the Cartesian coordinates to the
%%        spherical coordinates. This can only be done if the substrate is 3d.
%% @end
%%----------------------------------------------------------------------------
-spec get_substrate_cpps(integer(), abcn | iterative | none) -> [models:sensor()].
get_substrate_cpps(Dimensions, Plasticity) ->
  case (Plasticity == iterative) or (Plasticity == abcn) of
    true ->
      Std = [
        models:sensor(#{
          id => undefined,
          name => cartesian,
          type => substrate,
          cx_id => undefined,
          scape => undefined,
          vl => (Dimensions * 2 + 3),
          fanout_ids => [],
          generation => undefined,
          format => undefined,
          parameters => undefined
        }),
        models:sensor(#{
          id => undefined,
          name => centripital_distances,
          type => substrate,
          cx_id => undefined,
          scape => undefined,
          vl => (2 + 3),
          fanout_ids => [],
          generation => undefined,
          format => undefined,
          parameters => undefined
        }),
        models:sensor(#{
          id => undefined,
          name => cartesian_distance,
          type => substrate,
          cx_id => undefined,
          scape => undefined,
          vl => (1 + 3),
          fanout_ids => [],
          generation => undefined,
          format => undefined,
          parameters => undefined
        }),
        models:sensor(#{
          id => undefined,
          name => cartesian_coord_diffs,
          type => substrate,
          cx_id => undefined,
          scape => undefined,
          vl => (Dimensions + 3),
          fanout_ids => [],
          generation => undefined,
          format => undefined,
          parameters => undefined
        }),
        models:sensor(#{
          id => undefined,
          name => cartesian_gaussed_coord_diffs,
          type => substrate,
          cx_id => undefined,
          scape => undefined,
          vl => (Dimensions + 3),
          fanout_ids => [],
          generation => undefined,
          format => undefined,
          parameters => undefined
        }),
        models:sensor(#{
          id => undefined,
          name => iow,
          type => substrate,
          cx_id => undefined,
          scape => undefined,
          vl => 3,
          fanout_ids => [],
          generation => undefined,
          format => undefined,
          parameters => undefined
        })
      ],
      Adt = case Dimensions of
        2 ->
          [
            models:sensor(#{
              id => undefined,
              name => polar,
              type => substrate,
              cx_id => undefined,
              scape => undefined,
              vl => (Dimensions * 2 + 3),
              fanout_ids => [],
              generation => undefined,
              format => undefined,
              parameters => undefined
            })
          ];
        3 ->
          [
            models:sensor(#{
              id => undefined,
              name => spherical,
              type => substrate,
              cx_id => undefined,
              scape => undefined,
              vl => (Dimensions * 2 + 3),
              fanout_ids => [],
              generation => undefined,
              format => undefined,
              parameters => undefined
            })
          ];
        _ ->
          []
      end,
      lists:append(Std, Adt);
    false ->
      case (Plasticity == none) of
        true ->
          Std = [
            models:sensor(#{
              id => undefined,
              name => cartesian,
              type => substrate,
              cx_id => undefined,
              scape => undefined,
              vl => (Dimensions * 2),
              fanout_ids => [],
              generation => undefined,
              format => undefined,
              parameters => undefined
            }),
            models:sensor(#{
              id => undefined,
              name => centripital_distances,
              type => substrate,
              cx_id => undefined,
              scape => undefined,
              vl => 2,
              fanout_ids => [],
              generation => undefined,
              format => undefined,
              parameters => undefined
            }),
            models:sensor(#{
              id => undefined,
              name => cartesian_distance,
              type => substrate,
              cx_id => undefined,
              scape => undefined,
              vl => 1,
              fanout_ids => [],
              generation => undefined,
              format => undefined,
              parameters => undefined
            }),
            models:sensor(#{
              id => undefined,
              name => cartesian_coord_diffs,
              type => substrate,
              cx_id => undefined,
              scape => undefined,
              vl => Dimensions,
              fanout_ids => [],
              generation => undefined,
              format => undefined,
              parameters => undefined
            }),
            models:sensor(#{
              id => undefined,
              name => cartesian_gaussed_coord_diffs,
              type => substrate,
              cx_id => undefined,
              scape => undefined,
              vl => Dimensions,
              fanout_ids => [],
              generation => undefined,
              format => undefined,
              parameters => undefined
            })
          ],
          Adt = case Dimensions of
            2 ->
              [
                models:sensor(#{
                  id => undefined,
                  name => polar,
                  type => substrate,
                  cx_id => undefined,
                  scape => undefined,
                  vl => (Dimensions * 2),
                  fanout_ids => [],
                  generation => undefined,
                  format => undefined,
                  parameters => undefined
                })
              ];
            3 ->
              [
                models:sensor(#{
                  id => undefined,
                  name => spherical,
                  type => substrate,
                  cx_id => undefined,
                  scape => undefined,
                  vl => (Dimensions * 2),
                  fanout_ids => [],
                  generation => undefined,
                  format => undefined,
                  parameters => undefined
                })
              ];
            _ ->
              []
          end,
          lists:append(Std, Adt)
      end
  end.

%%-----------------------------------------------------------------------------
%% @doc The get_substrate_ceps starts the population off with the NN based
%%      agents using substrate_ceps determined by the Plasticity.
%% @end
%%-----------------------------------------------------------------------------
-spec get_substrate_ceps(integer(), abcn | iterative | none) -> [models:actuator()].
get_substrate_ceps(_Dimensions, Plasticity) ->
  case Plasticity of
    iterative ->
      [
        models:actuator(#{
          id => undefined,
          name => delta_weight,
          type => substrate,
          cx_id => undefined,
          scape => undefined,
          vl => 1,
          fanin_ids => [],
          generation => undefined,
          format => undefined,
          parameters => undefined
        })
      ];
    abcn ->
      [
        models:actuator(#{
          id => undefined,
          name => set_abcn,
          type => substrate,
          cx_id => undefined,
          scape => undefined,
          vl => 5,
          fanin_ids => [],
          generation => undefined,
          format => undefined,
          parameters => undefined
        })
      ];
    none ->
      [
        models:actuator(#{
          id => undefined,
          name => set_weight,
          type => substrate,
          cx_id => undefined,
          scape => undefined,
          vl => 1,
          fanin_ids => [],
          generation => undefined,
          format => undefined,
          parameters => undefined
        })
      ]
  end.

%%%============================================================================
%%% Internal functions
%%%============================================================================