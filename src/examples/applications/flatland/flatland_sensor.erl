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
%%% @doc Sensors for the Flatland example application.
%%% @end
%%%----------------------------------------------------------------------------
-module(flatland_sensor).

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

-record(avatar, {
  id,
  sector,
  morphology,
  type,
  energy = 0,
  health = 0,
  food = 0,
  age = 0,
  kills = 0,
  loc,
  direction,
  r,
  mass,
  objects,
  actuators,
  sensors
}).

-type state() :: {}.

%%%============================================================================
%%% Sensor callbacks
%%%============================================================================

%%-----------------------------------------------------------------------------
%% @private
%% @doc Initializes the sensor process.
%% @end
%%-----------------------------------------------------------------------------
-spec init([]) -> {ok, {}}.
init([]) ->
  {ok, {}}.

%%----------------------------------------------------------------------------
%% @doc sense(distance_scanner...) contacts the flatland simulator for
%%      a list of avatars and calculates the distance between them and itself;
%%      sense(color_scanner...) contacts the flatland simulator for
%%      a list of simulators and then determines the color of each avatar.
%% @end
%%----------------------------------------------------------------------------
-spec sense(atom(), {models:agent_id(), integer(), [any()], pid(), models:sensor_id(),
  models:op_mode(), state()}) -> ok.
sense(distance_scanner, {AgentId, _VL, _Params, _Scape, _SensorId, _OpMode, _ModState}) ->
  scape_mgr_client:sense(AgentId, self(), [AgentId]),
  ok;
sense(color_scanner, {AgentId, _VL, _Params, _Scape, _SensorId, _OpMode, _ModState}) ->
  scape_mgr_client:sense(AgentId, self(), [AgentId]),
  ok.

%%-----------------------------------------------------------------------------
%% @doc percept(distance_scanner...) calculates the distance
%%      between the avatars requested by the sensor and itself;
%%      percept(color_scanner...) determines the color of each avatar requested
%%      by the sensor.
%% @end
%%-----------------------------------------------------------------------------
-spec percept(atom(), {float() | destroyed, models:agent_id(), integer(), [any()],
  state()}) -> {[float()], state()}.
percept(distance_scanner, {SensoryInput, AgentId, VL, [[Spread], [Density], _], ModState}) ->
  case SensoryInput of
    destroyed ->
      {lists:duplicate(VL, -1), ModState};
    Avatars ->
      case lists:keyfind(AgentId, 2, Avatars) of
        false ->
          {lists:duplicate(VL, -1), ModState};
        Self ->
          Self = lists:keyfind(AgentId, 2, Avatars),
          Loc = Self#avatar.loc,
          Direction = Self#avatar.direction,
          Result = distance_scanner(silent, {1, 0, 0}, Density, Spread, Loc, Direction,
            lists:keydelete(AgentId, 2, Avatars)),
          {Result, ModState}
      end
  end;
percept(color_scanner, {SensoryInput, AgentId, VL, [[Spread], [Density], _], ModState}) ->
  case SensoryInput of
    destroyed ->
      {lists:duplicate(VL, -1), ModState};
    Avatars ->
      case lists:keyfind(AgentId, 2, Avatars) of
        false ->
          {lists:duplicate(VL, -1), ModState};
        Self ->
          Self = lists:keyfind(AgentId, 2, Avatars),
          Loc = Self#avatar.loc,
          Direction = Self#avatar.direction,
          Result = color_scanner(silent, {1, 0, 0}, Density, Spread, Loc, Direction,
            lists:keydelete(AgentId, 2, Avatars)),
          {Result, ModState}
      end
  end.

%%%============================================================================
%%% Internal functions
%%%============================================================================

%%-----------------------------------------------------------------------------
%% @private
%% @doc Input: ViewAngle = Radian, Density = n, Gaze direction =
%%      {SensorLoc, Direction}.
%%      Output: List of ranges 1/Distance no intersection = -1, with angle
%%      starting with Gaze + (ViewAngle/2), and ending with
%%      (Gaze - ViewAngle/2), [Dist1...DistDensity].
%% @end
%%-----------------------------------------------------------------------------
distance_scanner(_Op, {_Zoom, _PanX, _PanY}, Density, Spread, Loc, Direction, Avatars) ->
  {Resolution, StartAngle} = case is_even(Density) of
    true ->
      R = Spread / Density,
      SA = (Density / 2) * R,
      SA2 = -SA + R / 2,
      {R, SA2};
    false ->
      R = Spread / Density,
      SA = trunc(Density / 2) * R,
      SA2 = -SA,
      {R, SA2}
  end,
  UnitRays = create_unit_rays(Direction, Density, Resolution, StartAngle, []),
  RangeScanList = compose_range_scan_list(Loc, UnitRays, Avatars, []),
  RangeScanList.

compose_range_scan_list(Loc, [Ray | UnitRays], Avatars, Acc) ->
  {Distance, _Color} = flatland_utils:shortest_intr_line({Loc, Ray}, Avatars, {inf, void}),
  compose_range_scan_list(Loc, UnitRays, Avatars, [Distance | Acc]);
compose_range_scan_list(_Loc, [], _Avatars, Acc) ->
  lists:reverse(Acc).

color_scanner(_Op, {_Zoom, _PanX, _PanY}, Density, Spread, Loc, Direction, Avatars) ->
  {Resolution, StartAngle} = case is_even(Density) of
    true ->
      R = Spread / Density,
      SA = (Density / 2) * R,
      SA2 = -SA + R / 2,
      {R, SA2};
    false ->
      R = Spread / Density,
      SA = trunc(Density / 2) * R,
      SA2 = -SA,
      {R, SA2}
  end,
  UnitRays = create_unit_rays(Direction, Density, Resolution, StartAngle, []),
  ColorScanList = compose_color_scan_list(Loc, UnitRays, Avatars, []),
  ColorScanList.
    
compose_color_scan_list(Loc, [Ray | UnitRays], Avatars, Acc) ->
  {_Distance, Color} = flatland_utils:shortest_intr_line({Loc, Ray}, Avatars, {inf, void}),
  compose_color_scan_list(Loc, UnitRays, Avatars, [Color | Acc]);
compose_color_scan_list(_Loc, [], _Avatars, Acc) ->
  lists:reverse(Acc).

create_unit_rays(_, 0, _, _, Acc) ->
  Acc;
create_unit_rays({X, Y}, Density, Resolution, Angle, Acc) ->
  UnitRay = {X * math:cos(Angle) - Y * math:sin(Angle), X * math:sin(Angle) + Y * math:cos(Angle)},
  create_unit_rays({X, Y}, Density - 1, Resolution, Angle + Resolution, [UnitRay | Acc]).

is_even(Val) ->
  case (Val rem 2) of
    0 ->
      true;
    _ ->
      false
  end.