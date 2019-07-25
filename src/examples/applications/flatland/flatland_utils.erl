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
%%% @doc Flatland Utility functions.
%%% @end
%%%----------------------------------------------------------------------------
-module(flatland_utils).

%% API
-export([
  shortest_intr_line/3
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

%%%============================================================================
%%% API
%%%============================================================================

shortest_intr_line(Gaze, [Avatar | Avatars], Val) ->
  shortest_intr_line(Gaze, Avatars, intr(Gaze, Avatar#avatar.objects, Val));
shortest_intr_line(_Gaze, [], {Distance, Color}) ->
  case Distance of
    inf ->
      {-1, 1};
    0.0 ->
      {-1, 1};
    _ ->
      {Distance, clr2val(Color)}
  end.

%%%============================================================================
%%% Internal functions
%%%============================================================================

intr(Gaze, [{circle, _Id, Color, _Pivot, C, R} | Objects], {Min, MinColor}) ->
  {S, D} = Gaze,
  [{Xc, Yc}] = C,
  {Xs, Ys} = S,
  {Xd, Yd} = D,
  {Xv, Yv} = {Xs - Xc, Ys - Yc},
  VdotD = Xv*Xd + Yv*Yd,
  Dis = math:pow(VdotD,2) - (Xv*Xv + Yv*Yv - R*R),
  Result = case Dis > 0 of
    false ->
      inf;
    true ->
      SqrtDis = math:sqrt(Dis),
      I1 = -VdotD - SqrtDis,
      I2 = -VdotD + SqrtDis,
      case (I1 > 0) and (I2 >0) of
        true ->
          erlang:min(I1,I2);
        false ->
          inf
      end
  end,
  {UMin, UMinColor} = case Result < Min of
    true ->
      {Result, Color};
    false ->
      {Min, MinColor}
  end,
  intr(Gaze, Objects, {UMin, UMinColor});
intr(Gaze, [{line, _Id, Color, _Pivot, [{X3,Y3}, {X4,Y4}], _Parameter}|Objects], {Min, MinColor}) ->
  {S,D} = Gaze,
  {X1,Y1} = S,
  {XD0,YD0} = D,
  PerpXD1 = Y4-Y3,
  PerpYD1 = -(X4-X3),
  PerpXD0 = YD0,
  PerpYD0 = -XD0,
  Result = case PerpXD1*XD0 + PerpYD1*YD0 of
    0.0 ->
      inf;
    Denom ->
      RayLength = ((PerpXD1 * (X3-X1)) + (PerpYD1 * (Y3-Y1)))/Denom,
      T = ((PerpXD0 * (X3-X1)) + (PerpYD0 * (Y3-Y1)))/Denom,      
      case (RayLength >= 0) and (T >= 0) and (T =< 1) of
        true ->
          RayLength;
        false ->
          inf
      end
  end,
  {UMin, UMinColor} = case Result < Min of
    true ->
      {Result, Color};
    false ->
      {Min, MinColor}
  end,
  intr(Gaze, Objects, {UMin, UMinColor});
intr(_Gaze, [], {Min, MinColor})->
  {Min, MinColor}.

clr2val(Color)->
  case Color of
    green -> -0.5; % plant
    blue -> 0; % prey
    red -> 0.5; % predator
    _ -> 1
  end.