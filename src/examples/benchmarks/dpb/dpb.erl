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
%%% @doc Double pole balancing scape for benchmarking purposes.
%%% @end
%%%----------------------------------------------------------------------------
-module(dpb).

-behaviour(private_scape).

%% API
-export([]).

%% Scape callbacks
-export([
  init/1,
  sense/2,
  actuate/4
]).

%%%============================================================================
%%% Types
%%%============================================================================

-record(pb_state, {
  cpos :: float(),
  cvel :: float(),
  p_angle1 :: float(),
  p_vel1 :: float(),
  p_angle2 :: float(),
  p_vel2 :: float(),
  time_step :: integer(),
  goal_time_steps :: integer(),
  max_time_steps :: integer(),
  fitness_acc :: float()
}).

-type state() :: #pb_state{}.

%%%============================================================================
%%% Scape callbacks
%%%============================================================================

%%%----------------------------------------------------------------------------
%%% @private
%%% @doc Initializes the sensor process.
%%% @end
%%%----------------------------------------------------------------------------
-spec init([]) -> {ok, state()}.
init([]) ->
  NewState = pb_sim(),
  {ok, NewState}.

%%%----------------------------------------------------------------------------
%%% @private
%%% @doc Returns sensory input.
%%% @end
%%%----------------------------------------------------------------------------
-spec sense([atom()], state() | undefined) -> {[float()], state()}.
sense([Parameter], S) ->
  ScapeState = case S of
    undefined ->
      pb_sim();
    PrevState ->
      PrevState
  end,
  PVel1 = ScapeState#pb_state.p_vel1,
  PVel2 = ScapeState#pb_state.p_vel2,
  Rad2Angle = 2 * math:pi() / 360,
  AngleLimit = Rad2Angle * 36,
  ScaledCPosition = functions:scale(ScapeState#pb_state.cpos, 2.4, -2.4),
  ScaledCVel = functions:scale(ScapeState#pb_state.cvel, 10.0, -10.0),
  ScaledPAngle1 = functions:scale(ScapeState#pb_state.p_angle1, AngleLimit, -AngleLimit),
  ScaledPAngle2 = functions:scale(ScapeState#pb_state.p_angle2, AngleLimit, -AngleLimit),
  SenseSignal = case Parameter of
    cpos -> [ScaledCPosition];
    cvel -> [ScaledCVel];
    pangle1 -> [ScaledPAngle1];
    pvel1 -> [PVel1];
    pangle2 -> [ScaledPAngle2];
    pvel2 -> [PVel2];
    3 -> [ScaledCPosition, ScaledPAngle1, ScaledPAngle2];
    6 -> [ScaledCPosition, ScaledCVel, ScaledPAngle1, ScaledPAngle2, PVel1, PVel2]
  end,
  {SenseSignal, ScapeState}.

%%%----------------------------------------------------------------------------
%%% @private
%%% @doc Performs output action.
%%% @end
%%%----------------------------------------------------------------------------
-spec actuate(atom(), {[float()], [integer()]}, models:agent_id(), state()) ->
{{[float()], 0 | 1 | goal_reached}, state() | undefined}.
actuate(push, {[DampingFlag, _DPBFlag], [F]}, _AgentId, S) ->
  CPosition = S#pb_state.cpos,
  CVel = S#pb_state.cvel,
  PAngle1 = S#pb_state.p_angle1,
  PVel1 = S#pb_state.p_vel1,
  TimeStep = S#pb_state.time_step,
  GoalTimeSteps = S#pb_state.goal_time_steps,
  MaxTimeSteps = S#pb_state.max_time_steps,
  AL = 2 * math:pi() * (36 / 360),
  {NextCPosition, NextCVel, NextPAngle1, NextPVel1, NextPAngle2, NextPVel2} =
  sm_double_pole(F * 10, CPosition, CVel, PAngle1, PVel1, S#pb_state.p_angle2,
    S#pb_state.p_vel2, 2),
  case (NextPAngle1 > AL) or (NextPAngle1 < -AL) or (NextPAngle2 > AL) or (NextPAngle2 < -AL) or
    (CPosition > 2.4) or (CPosition < -2.4) or (TimeStep >= MaxTimeSteps) of
      true ->
        US = undefined,
        case TimeStep >= GoalTimeSteps of
          true ->
            {{[0.0], goal_reached}, US};
          false ->
            {{[0.0], 1}, US}
        end;
      false ->
        Fitness = case DampingFlag of
          without_damping ->
            1.0;
          with_damping ->
            Fitness1 = TimeStep / 1000,
            Fitness2 = case TimeStep < 100 of
              true ->
                0.0;
              false ->
                0.75 / (abs(CPosition) + abs(CVel) + abs(PAngle1) + abs(PVel1))
            end,
            Fitness1 * 0.1 + Fitness2 * 0.9
        end,
        UFitnessAcc = S#pb_state.fitness_acc + Fitness,
        USS = S#pb_state{cpos = NextCPosition, cvel = NextCVel,
        p_angle1 = NextPAngle1, p_vel1 = NextPVel1, p_angle2 = NextPAngle2, p_vel2 = NextPVel2,
        time_step = TimeStep + 1, goal_time_steps = GoalTimeSteps, max_time_steps = MaxTimeSteps,
        fitness_acc = UFitnessAcc},
        {{[Fitness], 0}, USS}
  end.

%%%============================================================================
%%% Internal functions
%%%============================================================================

pb_sim() ->
  utils:random_seed(),
  Angle1 = 3.6 * (2 * math:pi() / 360),
  Angle2 = 0.0,
  InitState = #pb_state{
    cpos = 0.0,
    cvel = 0.0,
    p_angle1 = Angle1,
    p_vel1 = 0.0,
    p_angle2 = Angle2,
    p_vel2 = 0.0,
    time_step = 1,
    goal_time_steps = 100000,
    max_time_steps = 100000,
    fitness_acc = 0.0
  },
  InitState.

sm_double_pole(_F, CPosition, CVel, PAngle1, PVel1, PAngle2, PVel2, 0) ->
  {CPosition, CVel, PAngle1, PVel1, PAngle2, PVel2};
sm_double_pole(F, CPosition, CVel, PAngle1, PVel1, PAngle2, PVel2, TimeSteps) ->
  PHalfLength1 = 0.5,
  PHalfLength2 = 0.05,
  M = 1, %CartMass
  PMass1 = 0.1,
  PMass2 = 0.01,
  MUc = 0.0005, %CartTrackFrictionCoefficient
  MUp = 0.000002, %PoleHingeFrictionCoefficient
  G = -9.81,
  Delta = 0.01,
  EM1 = PMass1 * (1 - (3 / 4) * math:pow(math:cos(PAngle1), 2)),
  EM2 = PMass2 * (1 - (3 / 4) * math:pow(math:cos(PAngle2), 2)),
  EF1 = PMass1 * PHalfLength1 * math:pow(PVel1, 2) * math:sin(PAngle1) + (3 / 4) *
  PMass1 * math:cos(PAngle1) * (((MUp * PVel1) / (PMass1 * PHalfLength1)) + G * math:sin(PAngle1)),
  EF2 = PMass2 * PHalfLength2 * math:pow(PVel2, 2) * math:sin(PAngle2) + (3 / 4) * PMass2 *
  math:cos(PAngle2) * (((MUp * PVel2) / (PMass1 * PHalfLength2)) + G * math:sin(PAngle2)),
  NextCAccel = (F - MUc * functions:sgn(CVel) + EF1 + EF2) / (M + EM1 + EM2),
  NextPAccel1 = - (3 / (4 * PHalfLength1)) * ((NextCAccel * math:cos(PAngle1)) +
    (G * math:sin(PAngle1)) + ((MUp * PVel1) / (PMass1 * PHalfLength1))),
  NextPAccel2 = -(3 / (4 * PHalfLength2)) * ((NextCAccel * math:cos(PAngle2)) +
    (G * math:sin(PAngle2)) + ((MUp * PVel2) / (PMass2 * PHalfLength2))),
  NextCVel = CVel + (Delta * NextCAccel),
  NextCPosition = CPosition + (Delta * CVel),
  NextPVel1 = PVel1 + (Delta * NextPAccel1),
  NextPAngle1 = PAngle1 + (Delta * NextPVel1),
  NextPVel2 = PVel2 + (Delta * NextPAccel2),
  NextPAngle2 = PAngle2 + (Delta * NextPVel2),
  sm_double_pole(0, NextCPosition, NextCVel, NextPAngle1, NextPVel1, NextPAngle2, NextPVel2,
    TimeSteps -1).