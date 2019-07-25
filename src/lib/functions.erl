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
%%% @doc The functions module contains the activation functions used by the
%%%      neuron, and other mathematical functions used by the system. Through
%%%      the functions module, the activation functions are fully decoupled
%%%      from the neurons using them. A neuron can use any activation
%%%      function, no matter its form, as long as it returns a properly
%%%      formatted value. NOTE: While the the activation functions are all
%%%      stored in the functions module, the aggregation and the plasticity
%%%      functions are stored in the signal_aggregator and plasticity modules
%%%      respectively.
%%% @end
%%%----------------------------------------------------------------------------
-module(functions).

%% API
-export([
  saturation/1, saturation/2,
  scale/3,
  sat/3,
  sat_dzone/5,
  tanh/1,
  relu/1,
  cos/1,
  sin/1,
  sgn/1,
  bin/1,
  trinary/1,
  multiquadric/1,
  absolute/1,
  linear/1,
  quadratic/1,
  gaussian/1, gaussian/2,
  sqrt/1,
  log/1,
  sigmoid/1,
  sigmoid1/1,
  avg/1,
  std/1, std/3,
  cartesian/2, cartesian/3,
  polar/2, polar/3,
  spherical/2, spherical/3,
  centripital_distances/2, centripital_distances/3,
  cartesian_distance/2, cartesian_distance/3,
  cartesian_coord_diffs/2, cartesian_coord_diffs/3,
  cartesian_gaussed_coord_diffs/2, cartesian_gaussed_coord_diffs/3,
  iow/3,
  to_cartesian/1,
  normalize/1,
  spherical2cartesian/1,
  cartesian2spherical/1,
  polar2cartesian/1,
  cartesian2polar/1,
  distance/2, distance/3,
  vector_difference/2, vector_difference/3
]).

%% Xref
-ignore_xref([
  saturation/1, saturation/2,
  scale/3,
  sat/3,
  sat_dzone/5,
  tanh/1,
  relu/1,
  cos/1,
  sin/1,
  sgn/1,
  bin/1,
  trinary/1,
  multiquadric/1,
  absolute/1,
  linear/1,
  quadratic/1,
  gaussian/1, gaussian/2,
  sqrt/1,
  log/1,
  sigmoid/1,
  sigmoid1/1,
  avg/1,
  std/1, std/3,
  cartesian/2, cartesian/3,
  polar/2, polar/3,
  spherical/2, spherical/3,
  centripital_distances/2, centripital_distances/3,
  cartesian_distance/2, cartesian_distance/3,
  cartesian_coord_diffs/2, cartesian_coord_diffs/3,
  cartesian_gaussed_coord_diffs/2, cartesian_gaussed_coord_diffs/3,
  iow/3,
  to_cartesian/1,
  normalize/1,
  spherical2cartesian/1,
  cartesian2spherical/1,
  polar2cartesian/1,
  cartesian2polar/1,
  distance/2, distance/3,
  vector_difference/2, vector_difference/3
]).

%%%============================================================================
%%% API
%%%============================================================================

%%-----------------------------------------------------------------------------
%% @doc The function saturation/1 accepts a value Val, and returns the same if
%%      its magnitude is below 1000. Otherwise it returns -1000 or 1000, if
%%      it’s less than or greater than -1000 or 1000 respectively. Thus Val
%%      saturates at -1000 and 1000.
%% @end
%%-----------------------------------------------------------------------------
-spec saturation(float()) -> float().
saturation(Val) ->
  case Val > 1000.0 of
    true ->
      1000.0;
    false ->
      case Val < -1000.0 of
        true ->
          -1000.0;
        false ->
          Val
      end
  end.

%%-----------------------------------------------------------------------------
%% @doc The saturation/2 function is similar to saturation/1, but here the
%%      spread (symmetric Max and Min values) is specified by the caller.
%% @end
%%-----------------------------------------------------------------------------
-spec saturation(float(), float()) -> float().
saturation(Val, Spread) ->
  case Val > Spread of
    true ->
      Spread;
    false ->
      case Val < -Spread of
        true ->
          -Spread;
        false ->
          Val
      end
  end.

%%-----------------------------------------------------------------------------
%% @doc The scale/3 function accepts a list of values, and scales them to be
%%      between the specified Min and Max values (No it does not).
%% @end
%%-----------------------------------------------------------------------------
-spec scale(float() | [float()], float(), float()) -> float().
scale([H | T], Max, Min) ->
  [scale(Val, Max, Min) || Val <- [H | T]];
scale(Val, Max, Min) ->
  case Max == Min of
    true ->
      0.0;
    false ->
      (Val * 2 - (Max + Min)) / (Max - Min)
  end.

%%-----------------------------------------------------------------------------
%% @doc The sat/3 function is similar to saturation/2 function, but here the
%%      Max and Min can be different, and are specified by the caller.
%% @end
%%-----------------------------------------------------------------------------
-spec sat(float(), float(), float()) -> float().
sat(Val, Min, _Max) when Val < Min ->
  Min;
sat(Val, _Min, Max) when Val > Max ->
  Max;
sat(Val, _Min, _Max) ->
  Val.

%%-----------------------------------------------------------------------------
%% @doc The sat_dzone/5 function is similar to the sat/3 function, but here,
%%      if Val is between DZMin and DZMax, it is zeroed.
%% @end
%%-----------------------------------------------------------------------------
-spec sat_dzone(float(), float(), float(), float(), float()) -> float().
sat_dzone(Val, Max, Min, DZMax, DZMin) ->
  case (Val < DZMax) and (Val > DZMin) of
    true ->
      0.0;
    false ->
      sat(Val, Max, Min)
  end.

%%%============================================================================
%%% Activation functions
%%%============================================================================

-spec tanh(float()) -> float().
tanh(Val) ->
  math:tanh(Val).

-spec relu(float()) -> float().
relu(Val) ->
  erlang:max(0.0, Val).

-spec cos(float()) -> float().
cos(Val) ->
  math:cos(Val).

-spec sin(float()) -> float().
sin(Val) ->
  math:sin(Val).

-spec sgn(float()) -> -1 | 0 | 1.
sgn(0) ->
  0;
sgn(Val) ->
  case Val > 0 of
    true -> 1;
    false -> -1
  end.

%%-----------------------------------------------------------------------------
%% @doc The bin/1 function converts Val into a binary value, 1 if Val > 0,
%%      and 0 if Val = < 0.
%% @end
%%-----------------------------------------------------------------------------
-spec bin(float()) -> 0 | 1.
bin(Val) ->
  case Val > 0 of
    true -> 1;
    false -> 0
  end.

%%-----------------------------------------------------------------------------
%% @doc The trinary/1 function converts Val into a trinary value.
%% @end
%%-----------------------------------------------------------------------------
-spec trinary(float()) -> -1 | 0 | 1.
trinary(Val) when (Val < 0.33) and (Val > -0.33) ->
  0;
trinary(Val) when Val >= 0.33 ->
  1;
trinary(Val) when Val =< -0.33 ->
  -1.

-spec multiquadric(float()) -> float().
multiquadric(Val) ->
  math:pow(Val * Val + 0.01, 0.5).

-spec absolute(float()) -> float().
absolute(Val) ->
  abs(Val).

-spec linear(float()) -> float().
linear(Val) ->
  Val.

-spec quadratic(float()) -> float().
quadratic(Val) ->
  sgn(Val) * Val * Val.

-spec gaussian(float()) -> float().
gaussian(Val) ->
  gaussian(2.71828183, Val).

-spec gaussian(float(), float()) -> float().
gaussian(Const, Val) ->
  V = case Val > 10.0 of
    true ->
      10.0;
    false ->
      case Val < -10.0 of
        true ->
          -10.0;
        false ->
          Val
      end
  end,
  math:pow(Const, -V * V).

-spec sqrt(float()) -> float().
sqrt(Val) ->
  sgn(Val) * math:sqrt(abs(Val)).

-spec log(float()) -> float().
log(Val) ->
  case Val == 0.0 of
    true ->
      0.0;
    false ->
      sgn(Val) * math:log(abs(Val))
  end.

-spec sigmoid(float()) -> float().
sigmoid(Val) ->
  V = case Val > 10.0 of
    true ->
      10.0;
    false ->
      case Val < -10.0 of
        true ->
          -10.0;
        false ->
          Val
      end
  end,
  1 / (1 + math:exp(-V)).

-spec sigmoid1(float()) -> float().
sigmoid1(Val) ->
  Val / (1 + abs(Val)).

%%-----------------------------------------------------------------------------
%% @doc The avg/1 function accepts a List for a parameter, and then returns
%%      the average of the list to the caller.
%% @end
%%-----------------------------------------------------------------------------
-spec avg([float()]) -> float().
avg(List) ->
  lists:sum(List) / length(List).

%%-----------------------------------------------------------------------------
%% @doc The std/1 function accepts a List for a parameter, and then returns to
%%      the caller the standard deviation of the list.
%% @end
%%-----------------------------------------------------------------------------
-spec std([float()]) -> float().
std(List) ->
  Avg = avg(List),
  std(List, Avg, []).

-spec std([float()], float(), [float()]) -> float().
std([Val | List], Avg, Acc) ->
  std(List, Avg, [math:pow(Avg - Val, 2.0) | Acc]);
std([], _Avg, Acc) ->
  Variance = lists:sum(Acc) / length(Acc),
  math:sqrt(Variance).

%%%============================================================================
%%% Coordinate operators
%%%============================================================================

-spec cartesian([float()], [float()]) -> [float()].
cartesian(ICoord, Coord) ->
  lists:append(ICoord, Coord).

-spec polar([float()], [float()]) -> [float()].
polar(ICoord, Coord) ->
  lists:append(cart2pol(ICoord), cart2pol(Coord)).

-spec spherical([float()], [float()]) -> [float()].
spherical(ICoord, Coord) ->
  lists:append(cart2spher(ICoord), cart2spher(Coord)).

-spec centripital_distances([float()], [float()]) -> [float()].
centripital_distances(ICoord, Coord) ->
  [centripital_distance(ICoord, 0.0), centripital_distance(Coord, 0.0)].

-spec cartesian_distance([float()], [float()]) -> [float()].
cartesian_distance(ICoord, Coord) ->
  [calculate_distance(ICoord, Coord, 0.0)].

-spec cartesian_coord_diffs([float()], [float()]) -> [float()].
cartesian_coord_diffs(ICoord, Coord) ->
  cartesian_coord_diffs1(ICoord, Coord, []).

-spec cartesian_gaussed_coord_diffs([float()], [float()]) -> [float()].
cartesian_gaussed_coord_diffs(FromCoords, ToCoords) ->
  cartesian_gaussed_coord_diffs1(FromCoords, ToCoords, []).

-spec cartesian_gaussed_coord_diffs1([float()], [float()], [float()]) -> [float()].
cartesian_gaussed_coord_diffs1([FromCoord | FromCoords], [ToCoord | ToCoords], Acc) ->
  cartesian_gaussed_coord_diffs1(FromCoords, ToCoords, [functions:gaussian(ToCoord-FromCoord)|Acc]);
cartesian_gaussed_coord_diffs1([], [], Acc) ->
  lists:reverse(Acc).

-spec cartesian([float()], [float()], [float()]) -> [float()].
cartesian(ICoord, Coord, [I, O, W]) ->
  [I, O, W | lists:append(ICoord, Coord)].

-spec polar([float()], [float()], [float()]) -> [float()].
polar(ICoord, Coord, [I, O, W]) ->
  [I, O, W | lists:append(cart2pol(ICoord), cart2pol(Coord))].

-spec spherical([float()], [float()], [float()]) -> [float()].
spherical(ICoord, Coord, [I, O, W]) ->
  [I, O, W | lists:append(cart2spher(ICoord), cart2spher(Coord))].

-spec centripital_distances([float()], [float()], [float()]) -> [float()].
centripital_distances(ICoord, Coord, [I, O, W]) ->
  [I, O, W, centripital_distance(ICoord, 0.0), centripital_distance(Coord, 0.0)].

-spec cartesian_distance([float()], [float()], [float()]) -> [float()].
cartesian_distance(ICoord, Coord, [I, O, W]) ->
  [I, O, W, calculate_distance(ICoord, Coord, 0.0)].

-spec cartesian_coord_diffs([float()], [float()], [float()]) -> [float()].
cartesian_coord_diffs(FromCoords, ToCoords, [I, O, W]) ->
  [I, O, W | cartesian_coord_diffs(FromCoords, ToCoords)].

-spec cartesian_gaussed_coord_diffs([float()], [float()], [float()]) -> [float()].
cartesian_gaussed_coord_diffs(FromCoords, ToCoords, [I, O, W]) ->
  [I, O, W | cartesian_gaussed_coord_diffs(FromCoords, ToCoords)].

-spec iow([float()], [float()], [float()]) -> [float()].
iow(_ICoord, _Coord, IOW) ->
  IOW.

-spec to_cartesian({cartesian, _} | {polar, {float(), float()}} |
  {spherical, {float(), float(), float()}}) -> {cartesian, _}.
to_cartesian(Direction) ->
  case Direction of
    {spherical, Coordinates} ->
      {cartesian, spherical2cartesian(Coordinates)};
    {polar, Coordinates} ->
      {cartesian, polar2cartesian(Coordinates)};
    {cartesian, Coordinates} ->
      {cartesian, Coordinates}
  end.

-spec normalize([float()]) -> [float()].
normalize(Vector) ->
  Normalizer = calculate_normalizer(Vector, 0.0),
  normalize(Vector, Normalizer, []).

-spec spherical2cartesian({float(), float(), float()}) -> {float(), float(), float()}.
spherical2cartesian({P, Theta, Phi}) ->
  X = P * math:sin(Phi) * math:cos(Theta),
  Y = P * math:sin(Phi) * math:sin(Theta),
  Z = P * math:cos(Phi),
  {X, Y, Z}.

-spec cartesian2spherical({float(), float()} | {float(), float(), float()}) ->
 {float(), float(), float()}.
cartesian2spherical({X, Y}) ->
  cartesian2spherical({X, Y, 0.0});
cartesian2spherical({X, Y, Z}) ->
  PreR = X*X + Y*Y,
  R = math:sqrt(PreR),
  P = math:sqrt(PreR + Z*Z),
  Theta = theta(R, X, Y),
  Phi = phi(P, Z),
  {P, Theta, Phi}.

-spec polar2cartesian({float(), float()}) -> {float(), float(), float()}.
polar2cartesian({R, Theta}) ->
  X = R * math:cos(Theta),
  Y = R * math:sin(Theta),
  {X, Y, 0.0}.

-spec cartesian2polar({float(), float()} | {float(), float(), _}) -> {float(), float()}.
cartesian2polar({X, Y}) ->
  cartesian2polar({X, Y, 0.0});
cartesian2polar({X, Y, _Z}) ->
  R = math:sqrt(X*X + Y*Y),
  Theta = theta(R, X, Y),
  {R, Theta}.

-spec distance([float()], [float()]) -> float().
distance(Vector1, Vector2) ->
  distance(Vector1, Vector2, 0.0).

-spec distance([float()], [float()], float()) -> float().
distance([Val1 | Vector1], [Val2 | Vector2], Acc) ->
  distance(Vector1, Vector2, Acc + math:pow(Val2 - Val1, 2.0));
distance([], [], Acc) ->
  math:sqrt(Acc).

-spec vector_difference([float()], [float()]) -> [float()].
vector_difference(Vector1, Vector2) ->
  vector_difference(Vector1, Vector2, []).

-spec vector_difference([float()], [float()], [float()]) -> [float()].
vector_difference([Val1 | Vector1], [Val2 | Vector2], Acc) ->
  vector_difference(Vector1, Vector2, [Val2 - Val1 | Acc]);
vector_difference([], [], Acc) ->
  lists:reverse(Acc).

%%%============================================================================
%%% Internal functions
%%%============================================================================

-spec phi(P :: float(), Z :: float()) -> float().
phi(P, Z) ->
  case P == 0.0 of
    false ->
      math:acos(Z / P);
    true ->
      0.0
  end.

-spec cartesian_coord_diffs1([float()], [float()], [float()]) -> [float()].
cartesian_coord_diffs1([FromCoord | FromCoords], [ToCoord | ToCoords], Acc) ->
  cartesian_coord_diffs1(FromCoords, ToCoords, [ToCoord - FromCoord|Acc]);
cartesian_coord_diffs1([], [], Acc) ->
  lists:reverse(Acc).

-spec cart2pol([float()]) -> [float()].
cart2pol([Y, X]) ->
  R = math:sqrt(X*X + Y*Y),
  Theta = theta(R, X, Y),
  [R, Theta].

-spec cart2spher([float()]) -> [float()].
cart2spher([Z, Y, X]) ->
  PreR = X*X + Y*Y,
  R = math:sqrt(PreR),
  P = math:sqrt(PreR + Z*Z),
  Theta = theta(R, X, Y),
  Phi = phi(P, Z),
  [P, Theta, Phi].

-spec theta(R :: float(), X :: float(), Y :: float()) -> float().
theta(R, X, Y) ->
  case R == 0.0 of
    true ->
      0.0;
    false ->
      theta_false_case(X, Y)
  end.

-spec theta_false_case(X :: float(), Y :: float()) -> float().
theta_false_case(X, Y) when (X > 0.0) and (Y >= 0.0) ->
  math:atan(Y / X);
theta_false_case(X, Y) when (X > 0.0) and (Y < 0.0) ->
  math:atan(Y / X) + 2 * math:pi();
theta_false_case(X, Y) when (X < 0.0) ->
  math:atan(Y / X) + math:pi();
theta_false_case(X, Y) when (X == 0.0) and (Y > 0.0) ->
  math:pi() / 2;
theta_false_case(X, Y) when (X == 0.0) and (Y < 0.0) ->
  3 * math:pi() / 2.

-spec centripital_distance([float()], float()) -> float().
centripital_distance([Val|Coord], Acc) ->
  centripital_distance(Coord, Val * Val + Acc);
centripital_distance([], Acc) ->
  math:sqrt(Acc).

-spec calculate_distance([float()], [float()], float()) -> float().
calculate_distance([Val1 | Coord1], [Val2 | Coord2], Acc) ->
  Distance = Val2 - Val1,
  calculate_distance(Coord1, Coord2, Distance * Distance + Acc);
calculate_distance([], [], Acc) ->
  math:sqrt(Acc).

-spec calculate_normalizer([float()], float()) -> float().
calculate_normalizer([Val | Vector], Acc) ->
  calculate_normalizer(Vector, Val * Val + Acc);
  calculate_normalizer([], Acc) ->
  math:sqrt(Acc).

-spec normalize([float()], float(), [float()]) -> [float()].
normalize([Val|Vector], Normalizer, Acc) ->
  normalize(Vector, Normalizer, [Val / Normalizer | Acc]);
  normalize([], _Normalizer, Acc) ->
  lists:reverse(Acc).