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
%%% @doc The Flatland scape.
%%% @end
%%%----------------------------------------------------------------------------
-module(flatland).

-behaviour(sector).

%% API
-export([]).

%% Scape callbacks
-export([
  init/1,
  enter/3,
  sense/4,
  actuate/4,
  leave/3,
  remove/2,
  insert/3
]).

%%%============================================================================
%%% Types
%%%============================================================================

-record(flatland_state, {
  type :: atom()
}).

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

-type state() :: #flatland_state{}.

%%%============================================================================
%%% Configuration
%%%============================================================================

-define(AVATARS, t1).
-define(MEMBERS, t2).

%%%============================================================================
%%% Scape callbacks
%%%============================================================================

%%-----------------------------------------------------------------------------
%% @private
%% @doc Initializes the Flatland server.
%% @end
%%-----------------------------------------------------------------------------
-spec init(atom()) -> {ok, state()}.
init(ScapeType) ->
  InitAvatars = world_init(),
  sector:store(?AVATARS, InitAvatars),
  InitState = #flatland_state{type = ScapeType},
  {ok, InitState}.

%%-----------------------------------------------------------------------------
%% @private
%% @doc Enter the public Flatland scape.
%% @end
%%-----------------------------------------------------------------------------
-spec enter(models:agent_id(), [{atom(), [models:sensor_id()], [models:actuator_id()]}], state())
-> {success, state()}.
enter(AgentId, [{Morphology, CF, CT}], State) ->
  case sector:fetch(?MEMBERS, AgentId) of
    [{AgentId, _Status}] ->
      logr:info({scape, enter, ok, "already registered citizen", [AgentId]}),
      {success, State};
    [] ->
      sector:store(?MEMBERS, {AgentId, entered}),
      {AvatarId, Avatar} = create_avatar(Morphology, {CF, CT}, AgentId),
      sector:store(?AVATARS, {AvatarId, Avatar}),
      {success, State}
  end.

%%-----------------------------------------------------------------------------
%% @private
%% @doc Returns list of avatars as sensory input.
%% @end
%%-----------------------------------------------------------------------------
-spec sense(models:agent_id(), any(), pid(), state() | undefined)
-> {destroyed | [float()], state()}.
sense(AgentId, _Params, _SensorPid, State) ->
  case sector:fetch(?MEMBERS, AgentId) of
    [{AgentId, destroyed}] ->
      {destroyed, State};
    _ ->
      Avatars = sector:fetch(?AVATARS),
      {Avatars, State}
  end.

%%-----------------------------------------------------------------------------
%% @private
%% @doc Performs output action.
%% @end
%%-----------------------------------------------------------------------------
-spec actuate(models:agent_id(), atom(), any(), state()) -> {{[float()], integer()}, state()}.
actuate(AgentId, two_wheels, Output, State) ->
  case sector:fetch(?MEMBERS, AgentId) of
    [] ->
      logr:info({scape, actuate, ok, "unrecognized citizen", [AgentId]}),
      {{[0.0], 0}, State};
    [{AgentId, destroyed}] ->
      sector:delete(?MEMBERS, AgentId),
      {{[0.0], 1}, State};
    [{AgentId, entered}] ->
      [{AgentId, Avatar}] = sector:fetch(?AVATARS, AgentId),
      UAvatar = two_wheels(Avatar#avatar{kills = 0}, Output),
      case (UAvatar#avatar.energy > 0) and (UAvatar#avatar.age < 20000) of
        true ->
          Fitness = 0.001 + Avatar#avatar.kills,
          sector:store(?AVATARS, UAvatar),
          Avatars = sector:fetch(?AVATARS),
          collision_detection(UAvatar, Avatars),
          {{[Fitness], 0}, State};
        false ->
          destroy_avatar(AgentId),
          {{[0.0], 1}, State}
      end
  end.

%%-----------------------------------------------------------------------------
%% @private
%% @doc Leave the public Flatland scape.
%% @end
%%-----------------------------------------------------------------------------
-spec leave(models:agent_id(), any(), state()) -> {ok, state()}.
leave(AgentId, _Params, State) ->
  destroy_avatar(AgentId),
  {ok, State}.

%%-----------------------------------------------------------------------------
%% @private
%% @doc Remove Agent from the public Flatland scape.
%% @end
%%-----------------------------------------------------------------------------
-spec remove(models:agent_id(), state()) -> {term(), state()}.
remove(_AgentId, State) ->
  {ok, State}.

%%-----------------------------------------------------------------------------
%% @private
%% @doc Insert Agent into the public Flatland scape.
%% @end
%%-----------------------------------------------------------------------------
-spec insert(models:agent_id(), term(), state()) -> {ok, state()}.
insert(_AgentId, ok, State) ->
  {ok, State}.

%%%============================================================================
%%% Internal functions
%%%============================================================================

%%-----------------------------------------------------------------------------
%% World
%%-----------------------------------------------------------------------------

world_init()->
  [create_avatar(plant) || _<- lists:duplicate(100, 1)].

world_behavior(Collision, Penetration, OAvatar, Avatar) ->
  OAType = OAvatar#avatar.type,
  AType = Avatar#avatar.type,
  if
    (OAType == prey) and (AType == plant) ->
      logr:info({scape, world_behavior, ok, "prey ate a plant",
        [OAvatar#avatar.id, Avatar#avatar.id]}),
      {Avatar#avatar.energy * 0.25, plant_eaten, OAvatar, Avatar};
    (OAType == prey) and (AType == prey) and (Collision == true) ->
      PushStrength = 0.1, % Push
      UAvatar = push(OAvatar, Avatar, PushStrength),
      {0, void, OAvatar, UAvatar};
    (OAType == predator) and (AType == prey) and (Penetration == true) ->
      logr:info({scape, world_behavior, ok, "predator ate a prey",
        [OAvatar#avatar.id, Avatar#avatar.id]}),
      {500, destroy, OAvatar, Avatar};
    (OAType == predator) and (AType == prey) and (Collision == true)->
      logr:info({scape, world_behavior, ok, "predator prey collision", []}),
      PushStrength = 1, % Push very hard
      UAvatar = push(OAvatar, Avatar, PushStrength),
      {0, void, OAvatar, UAvatar};
    (OAType == predator) and (AType == predator) and (Penetration == true) ->
      PushStrength = 0.5, % Push hard
      UAvatar = push(OAvatar, Avatar, PushStrength),
      {0, void, OAvatar, UAvatar};
    (OAType == predator) and (AType == predator) and (Collision == true) ->
      PushStrength = 0.2, % Push
      UAvatar = push(OAvatar, Avatar, PushStrength),
      {0, void, OAvatar, UAvatar};
    (OAType == predator) and (AType == plant) and (Collision == true) ->
      PushStrength = 0, % Push aside
      UAvatar = push(OAvatar, Avatar, PushStrength),
      {0, void, OAvatar, UAvatar};
    true ->
      {0, void, OAvatar, Avatar}
  end.

push(OAvatar, Avatar, PushStrength) ->
  OAEnergy = OAvatar#avatar.energy,
  AEnergy = Avatar#avatar.energy,
  case OAEnergy > AEnergy of
    true ->
      {OX, OY} = OAvatar#avatar.loc,
      {X, Y} = Avatar#avatar.loc,
      DX = X - OX,
      DY = Y - OY,
      Distance = math:sqrt(math:pow(OX - X, 2) + math:pow(OY - Y, 2)),
      MinDistance = OAvatar#avatar.r + Avatar#avatar.r,
      {MinPushX, MinPushY} = case Distance == 0 of
        true ->
          {-DX, -DY};
        false ->
          {(MinDistance/Distance)*DX - DX, (MinDistance/Distance)*DY - DY}
      end,
      PushX = MinPushX + case DX == 0 of
        true -> 0;
        false -> (DX/abs(DX)) * PushStrength
      end,
      PushY = MinPushY + case DY == 0 of
        true -> 0;
        false -> (DY/abs(DY)) * PushStrength
      end,
      ULoc = {X + PushX, Y + PushY},
    UObjects = [{ObjName, Id, Color, {PX + PushX, PY + PushY}, [{CX + PushX, CY + PushY} ||
    {CX, CY} <- Coords], P} || {ObjName, Id, Color, {PX, PY}, Coords, P} <- Avatar#avatar.objects],
      Avatar#avatar{loc = ULoc, energy = (AEnergy - 10 * PushStrength), objects = UObjects};
    false ->
      Avatar
  end.

%%-----------------------------------------------------------------------------
%% Collision detection
%%-----------------------------------------------------------------------------

collision_detection(OpAvatar, Avatars) ->
  collision_detection(OpAvatar, 0, 0, Avatars).

collision_detection(OpAvatar, EnergyAcc, Kills, [{_Id, Avatar} | Avatars]) ->
  if
    (Avatar#avatar.id == OpAvatar#avatar.id) ->
      collision_detection(OpAvatar, EnergyAcc, Kills, Avatars);
    true ->
      {X, Y} = OpAvatar#avatar.loc,
      {DX, DY} = OpAvatar#avatar.direction,
      {Xav, Yav} = Avatar#avatar.loc,
      Penetration = case (OpAvatar#avatar.type == predator) of
        true ->
          {InterDist, _Color} =
          SpearUnitRay = {DX*math:cos(0) - DY*math:sin(0), DX*math:sin(0) + DY*math:cos(0)},
          flatland_utils:shortest_intr_line({{X, Y}, SpearUnitRay}, [Avatar], {inf, void}),
          (InterDist =/= -1) and (InterDist < (2 + OpAvatar#avatar.r));
        false ->
          false
      end,
      Distance = math:sqrt(math:pow(X - Xav, 2) + math:pow(Y - Yav, 2)),
      Collision = (Distance < (OpAvatar#avatar.r + Avatar#avatar.r)),
      {Energy, Order, UOpAvatar, UAvatar} = case Collision or Penetration of
        true ->
          world_behavior(Collision, Penetration, OpAvatar, Avatar);
        false ->
          {0, void, OpAvatar, Avatar}
      end,
      case Order of
        destroy ->
          sector:store(?MEMBERS, {UAvatar#avatar.id, destroyed}),       
          collision_detection(UOpAvatar, EnergyAcc + Energy, Kills + 1, Avatars);
        plant_eaten ->
          KScore = case Energy > 0 of
            true ->
              1;
            false ->
              0
          end,
          case UAvatar#avatar.type of
            plant ->
              A = respawn_avatar(UAvatar),
              sector:store(?AVATARS, {UAvatar#avatar.id, A}),
              collision_detection(UOpAvatar, EnergyAcc+Energy, Kills+KScore, Avatars);
            _ ->
              collision_detection(UOpAvatar, EnergyAcc+Energy, Kills+KScore, Avatars)
          end;
        void ->
          collision_detection(UOpAvatar, EnergyAcc + Energy, Kills, Avatars)
      end
  end;
collision_detection(OpAvatar, EnergyAcc, KillsAcc, []) ->
  case EnergyAcc =/= 0 of
    true ->
      Energy = functions:saturation(OpAvatar#avatar.energy + EnergyAcc, 10000.0),
      UOpAvatar = OpAvatar#avatar{energy = Energy, kills = OpAvatar#avatar.kills + KillsAcc},
      sector:store(?AVATARS, {UOpAvatar#avatar.id, UOpAvatar});
    false ->
      sector:store(?AVATARS, {OpAvatar#avatar.id, OpAvatar})
  end.

%%-----------------------------------------------------------------------------
%% Movement
%%-----------------------------------------------------------------------------

two_wheels(Avatar, [SWheel1, SWheel2]) ->
  {Speed, Angle} = twowheel_to_moverotate(SWheel1, SWheel2),
  Rotated_Avatar = rotate(Avatar, Angle),
  Moved_Avatar = move(Rotated_Avatar, Speed),
  AgeAcc = Moved_Avatar#avatar.age,
  Moved_Avatar#avatar{age = AgeAcc + 1}.

twowheel_to_moverotate(Wr, Wl) ->
  Speed = (Wr + Wl)/2,
  Angle = Wr - Wl,
  {Speed, Angle}.

move(Avatar, S) ->
  {LX, LY} = Avatar#avatar.loc,
  {DX, DY} = Avatar#avatar.direction,
  Speed = case Avatar#avatar.type of
    prey ->
      S;
    _ ->
      S*0.9
  end,
  Energy = Avatar#avatar.energy,
  UEnergy = Energy - 0.1*(math:sqrt(math:pow(DX*Speed, 2)+math:pow(DY*Speed, 2)))-0.1,
  ULoc = {LX+(DX*Speed), LY+(DY*Speed)},
  UObjects = [{ObjName, Id, C, {PX+(DX*Speed), PY+(DY*Speed)}, [{X+(DX*Speed), Y+(DY*Speed)} ||
  {X, Y} <- Coords], P} || {ObjName, Id, C, {PX, PY}, Coords, P} <- Avatar#avatar.objects],
  Avatar#avatar{energy = UEnergy, loc = ULoc, objects = UObjects}.

rotate(Avatar, A) ->
  Ratio = math:pi()/4,
  Angle = A*Ratio,
  {DX, DY} = Avatar#avatar.direction,
  Energy = Avatar#avatar.energy,
  UEnergy = Energy - 0.1*(abs(Angle))-0.1,
  UDirection = {DX*math:cos(Angle) - DY*math:sin(Angle), DX*math:sin(Angle) + DY*math:cos(Angle)},
  UObjects = rotation(Avatar#avatar.objects, Angle, []),
  Avatar#avatar{energy = UEnergy, direction = UDirection, objects = UObjects}.
  
rotation([Object|Objects], Angle, Acc) ->
  {ObjName, Id, Color, {PX, PY}, Coords, Parameter} = Object,
  U_Coords = [{(X-PX)*math:cos(Angle) - (Y-PY)*math:sin(Angle) + PX,
  (X-PX)*math:sin(Angle) + (Y-PY)*math:cos(Angle) +PY} || {X, Y} <- Coords],
  U_Object = {ObjName, Id, Color, {PX, PY}, U_Coords, Parameter},
  rotation(Objects, Angle, [U_Object|Acc]);
rotation([], _Angle, Acc) ->
  Acc.

%%-----------------------------------------------------------------------------
%% Avatars
%%-----------------------------------------------------------------------------

create_avatar(Morphology, {CF, CT}, AvatarId)
when (Morphology == predator) or (Morphology == prey) ->
  case Morphology of
    predator ->
      Color = red,
      Loc = {X, Y} = {rand:uniform(400) + 300, rand:uniform(400) + 300},
      Direction = {DX, DY} = {-1/math:sqrt(2), -1/math:sqrt(2)},
      Energy = 1000,
      Mass = 6,
      R = 6,
      Objects = [{circle, undefined, Color, {X, Y}, [{X, Y}], R},
      {line, undefined, red, {X, Y}, [{X, Y}, {X+DX*R*2, Y+DY*R*2}], void}],
      Avatar = #avatar{
        id = AvatarId,
        type = Morphology,
        energy = Energy,
        loc = Loc,
        direction = Direction,
        r = R,
        mass = Mass,
        objects = Objects,
        actuators = CF,
        sensors = CT
      },
      {AvatarId, Avatar};
    prey ->
      Direction = {1/math:sqrt(2), 1/math:sqrt(2)},
      {X, Y} = {rand:uniform(800), rand:uniform(500)},
      Energy = 1000,
      Mass = 10,
      R = 10,
      Color = blue,
      Objects = [{circle, undefined, Color, {X, Y}, [{X, Y}], R}],
      Avatar = #avatar{
        id = AvatarId,
        type = Morphology,
        energy = Energy,
        loc = {X, Y},
        direction = Direction,
        r = R,
        mass = Mass,
        objects = Objects,
        actuators = CF,
        sensors = CT
      },
      {AvatarId, Avatar}
  end.

create_avatar(Morphology) when Morphology == plant ->
  AvatarId = gen_id(),
  Direction = {1/math:sqrt(2), 1/math:sqrt(2)},
  {X, Y} = Loc = {rand:uniform(800), rand:uniform(500)},
  Energy = 500,
  Mass = 3,
  R = 3,
  Objects = [{circle, undefined, green, {X, Y}, [{X, Y}], R}],
  Avatar = #avatar{
    id = AvatarId,
    type = Morphology,
    energy = Energy,
    food = 0,
    health = 0,
    mass = Mass,
    loc = Loc,
    direction = Direction,
    r = R,
    objects = Objects
  },
  {AvatarId, Avatar}.

destroy_avatar(AgentId) ->
  case sector:fetch(?MEMBERS, AgentId) of
    [] ->
      ok;
    [{AgentId, _Status}] -> 
      sector:delete(?MEMBERS, AgentId)
  end,
  sector:delete(?AVATARS, AgentId).

respawn_avatar(A) ->
  {X, Y} = {rand:uniform(800), rand:uniform(500)},
  A#avatar{
    loc = {X, Y},
    energy = 500,
    objects = [
      {circle, Id, green, {X, Y}, [{X, Y}], R} ||
      {circle, Id, _Color, {OldX, OldY}, [{OldX, OldY}], R} <- A#avatar.objects]
  }.

gen_id() ->
  1 / rand:uniform() * 1000000 / 1000000.