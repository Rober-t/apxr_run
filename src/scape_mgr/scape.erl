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
%%% @doc Scapes are self contained simulated worlds or virtual environments,
%%%      that is, they are not necessarily physical. They can be thought of as
%%%      a way of interfacing with the problem in question. Scapes are composed
%%%      of two parts, a simulation of an environment or a problem we are
%%%      applying the NN to, and a function that can keep track of the NN’s
%%%      performance. Scapes run outside the NN systems, as independent
%%%      processes with which the NNs interact using their sensors and
%%%      actuators. There are two types of scapes. One type of scape, private,
%%%      is spawned for each NN during the NN’s creation, and destroyed when
%%%      that NN is taken offline. Another type of scape, public, is
%%%      persistent, they exist regardless of the NNs, and allow multiple NNs
%%%      to interact with them at the same time, and thus they can allow those
%%%      NNs to interact with each other too. This module defines the public
%%%      scape.
%%% @end
%%%----------------------------------------------------------------------------
-module(scape).

-behaviour(gen_server).

%% Start/Stop
-export([
  start_link/5
]).

%% API
-export([
  enter/2,
  sense/3,
  actuate/4,
  leave/2,
  query_area/4,
  insert/3,
  move/3,
  whereis/1
]).

%% gen_server callbacks
-export([
  init/1,
  handle_call/3,
  handle_cast/2,
  terminate/2
]).

%% Xref
-ignore_xref([
  start_link/5,
  query_area/4,
  insert/3,
  move/3,
  whereis/1
]).

%%%============================================================================
%%% Types
%%%============================================================================

-record(scape_state, {
  mod_name :: atom()
}).

-type state() :: #scape_state{}.

%%%============================================================================
%%% QT Records
%%%============================================================================

-record(xy_point, {
  x :: float(),
  y :: float(),
  point :: {float(), float()},
  agent_id :: models:agent_id() | undefined
}).

-record(bounding_box, {
  x :: float(),
  y :: float(),
  width :: float(),
  height :: float(),
  min_x :: float(),
  min_y :: float(),
  max_x :: float(),
  max_y :: float()
}).

-record(quad_node, {
  uid :: integer() | root,
  bb :: bounding_box(),
  points = [] :: [xy_point()] | [],
  height :: non_neg_integer(),
  north_west :: non_neg_integer() | undefined,
  north_east :: non_neg_integer() | undefined,
  south_west :: non_neg_integer() | undefined,
  south_east :: non_neg_integer() | undefined,
  mod_name :: atom()
}).

-type xy_point() :: #xy_point{}.
-type bounding_box() :: #bounding_box{}.
-type quad_node() :: #quad_node{}.

%%%============================================================================
%%% QT Configuration
%%%============================================================================

-define(MAX_CAPACITY, 50). % Max number of children before sub-dividing

%%%============================================================================
%%% API
%%%============================================================================

%%-----------------------------------------------------------------------------
%% @doc The start_link function spawns the Scape process.
%% @end
%%-----------------------------------------------------------------------------
-spec start_link(X :: float(), Y :: float(), W :: float(), H :: float(), ModName :: atom()) ->
{ok, pid()}.
start_link(X, Y, Width, Height, ModName) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, {X, Y, Width, Height, ModName}, []).

%%-----------------------------------------------------------------------------
%% @doc Enter public scape.
%% @end
%%-----------------------------------------------------------------------------
-spec enter(models:agent_id(), any()) -> ok.
enter(AgentId, Params) ->
  gen_server:cast(?MODULE, {enter, AgentId, Params}).

%%-----------------------------------------------------------------------------
%% @doc Gather sensory inputs from the environment (Public Scape).
%% @end
%%-----------------------------------------------------------------------------
-spec sense(models:agent_id(), pid(), any()) -> ok.
sense(AgentId, SensorPid, Params) ->
  gen_server:cast(?MODULE, {sense, AgentId, SensorPid, Params}).

%%-----------------------------------------------------------------------------
%% @doc Perform various scape functions e.g. move, push, etc. The scape
%%      API is problem dependent. This function provides an interface
%%      to call various functions defined by the scape in question
%%      (Public Scape).
%% @end
%%-----------------------------------------------------------------------------
-spec actuate(models:agent_id(), pid(), atom(), any()) -> ok.
actuate(AgentId, ActuatorPid, Function, Params) ->
  gen_server:cast(?MODULE, {actuate, AgentId, ActuatorPid, Function, Params}).

%%-----------------------------------------------------------------------------
%% @doc Leave public scape.
%% @end
%%-----------------------------------------------------------------------------
-spec leave(models:agent_id(), any()) -> ok.
leave(AgentId, Params) ->
  gen_server:cast(?MODULE, {leave, AgentId, Params}).

%%-----------------------------------------------------------------------------
%% @doc Query a specific area within the scape.
%% @end
%%-----------------------------------------------------------------------------
-spec query_area(float(), float(), float(), float()) -> list().
query_area(X, Y, W, H) ->
  gen_server:call(?MODULE, {query_area, X, Y, W, H}).

%-----------------------------------------------------------------------------
% @private
% @doc Insert point at X,Y into tree.
% @end
%-----------------------------------------------------------------------------
-spec insert(float(), float(), models:agent_id()) -> boolean().
insert(X, Y, AgentId) ->
  gen_server:call(?MODULE, {insert, X, Y, AgentId}).

%-----------------------------------------------------------------------------
% @private
% @doc Move the AgentId to a different point in the tree.
% @end
%-----------------------------------------------------------------------------
-spec move(float(), float(), models:agent_id()) -> boolean().
move(X, Y, AgentId) ->
 gen_server:call(?MODULE, {move, X, Y, AgentId}).

%-----------------------------------------------------------------------------
% @private
% @doc Lookup AgentId in tree.
% @end
%-----------------------------------------------------------------------------
-spec whereis(models:agent_id()) -> {float(), float()} | not_found.
whereis(AgentId) ->
  gen_server:call(?MODULE, {whereis, AgentId}).

%%%============================================================================
%%% gen_server callbacks
%%%============================================================================

%%-----------------------------------------------------------------------------
%% @private
%% @doc Initializes the Scape server.
%% @end
%%-----------------------------------------------------------------------------
init({X, Y, Width, Height, ModName}) ->
  process_flag(trap_exit, true),
  utils:random_seed(),
  new(X, Y, Width, Height, ModName),
  logr:debug({scape, init, ok, undefined, [ModName]}),
  S = #scape_state{mod_name = ModName},
  {ok, S}.

%%-----------------------------------------------------------------------------
%% @private
%% @doc Handle call messages.
%% @end
%%-----------------------------------------------------------------------------
-spec handle_call(term(), {pid(), term()}, state()) -> {reply, term(), state()}.
handle_call({query_area, X, Y, W, H}, _F, State) ->
  Results = query_range(X, Y, W, H),
  {reply, Results, State};

handle_call({insert, X, Y, AgentId}, _F, State) ->
  XYP = build_xy_point(X, Y, AgentId),
  RootNode = get_root(),
  Result = do_insert(XYP, RootNode),
  {reply, Result, State};

handle_call({move, X, Y, AgentId}, _F, State) ->
  Result = do_move(X, Y, AgentId),
  {reply, Result, State};

handle_call({whereis, AgentId}, _F, State) ->
  Result = do_whereis(AgentId),
  {reply, Result, State};

handle_call(Request, From, State) ->
  logr:warning({scape, msg, error, "unexpected handle_call", [Request, From]}),
  {reply, ok, State}.

%%-----------------------------------------------------------------------------
%% @private
%% @doc Handle cast messages.
%% @end
%%-----------------------------------------------------------------------------
-spec handle_cast({enter, models:agent_id(), _} | {leave, models:agent_id(), _} |
  {sense, models:agent_id(), pid(), float()} |
  {actuate, models:agent_id(), pid(), atom(), float()} |
  {query_area, float(), float(), float(), float()}, state())
-> {noreply, state()}.
handle_cast({enter, AgentId, Params}, State) ->
  do_enter(AgentId, Params, State),
  {noreply, State};

handle_cast({sense, AgentId, SensorPid, Params}, State) ->
  do_sense(AgentId, Params, SensorPid, State),
  {noreply, State};

handle_cast({actuate, AgentId, ActuatorPid, Function, Params}, State) ->
  do_actuate(AgentId, Function, ActuatorPid, Params, State),
  {noreply, State};

handle_cast({leave, AgentId, Params}, State) ->
  do_leave(AgentId, Params, State),
  {noreply, State}.

%%-----------------------------------------------------------------------------
%% @private
%% @doc This function is called by a gen_server when it is about to terminate.
%%      It should be the opposite of Module:init/1 and do any necessary
%%      cleaning up. When it returns, the gen_server terminates with Reason.
%%      The return value is ignored.
%% @end
%%-----------------------------------------------------------------------------
-spec terminate(any(), state()) -> ok.
terminate(Reason, _State) ->
  logr:debug({scape, terminate, ok, undefined, [Reason]}),
  case Reason of
    shutdown ->
      stop_sectors(),
      try {
        ets:delete_all_objects(ids_sids_loc),
        ets:delete_all_objects(xy_pts),
        ets:delete_all_objects(qt)
      }
      catch
        error:badarg ->
          ok
      end;
    _ ->
      ok
  end.

%%%============================================================================
%%% Internal functions
%%%============================================================================

do_enter(AgentId, Params, _S) ->
  true = join(AgentId),
  SectorUId = fetch_sector(AgentId),
  sector:enter(SectorUId, AgentId, Params).

do_sense(AgentId, Params, SensorPid, _S) ->
  SectorUId = fetch_sector(AgentId),
  sector:sense(SectorUId, AgentId, SensorPid, Params).

do_actuate(AgentId, Function, ActuatorPid, Params, _S) ->
  SectorUId = fetch_sector(AgentId),
  sector:actuate(SectorUId, AgentId, Function, ActuatorPid, Params).

do_leave(AgentId, Params, _S) ->
  SectorUId = fetch_sector(AgentId),
  true = remove(AgentId),
  sector:leave(SectorUId, AgentId, Params).

%%%============================================================================
%%% QT API
%%%============================================================================
%%% @private
%%% @doc A quad tree is a data structure comprising nodes which store a bucket
%%%      of points and a bounding box. Any point which is contained within the
%%%      node's bounding box is added to its bucket. Once the bucket gets
%%%      filled up, the node splits itself into four nodes with a bounding box
%%%      corresponding to a quadrant of its parents bounding box. All points
%%%      which would have gone into the parent's bucket now go into one of it`s
%%%      children's buckets.
%%% @end
%%%============================================================================

%%-----------------------------------------------------------------------------
%% @private
%% @doc Create a quad tree who's upper left coordinate is located at x, y and
%%      it's bounding box is described by the height and width. This uses a
%%      default leaf_capacity of 10 and a max_tree_height of 20.
%% @end
%%-----------------------------------------------------------------------------
-spec new(X :: float(), Y :: float(), Width :: float(), Height :: float(), ModName :: atom()) ->
boolean().
new(X, Y, Width, Height, ModName) ->
  XYP = build_xy_point(X, Y),
  BB = build_bounding_box(XYP, Width, Height),
  RootNode = build_root_quad_node(BB, ModName),
  ets:insert(qt, {RootNode#quad_node.uid, RootNode}).

%%-----------------------------------------------------------------------------
%% @private
%% @doc get_root Get the root node.
%% @end
%%-----------------------------------------------------------------------------
-spec get_root() -> quad_node().
get_root() ->
  [{root, RootNode}] = ets:lookup(qt, root),
  RootNode.

%%-----------------------------------------------------------------------------
%% @private
%% @doc join Inserts a random point at X,Y into tree.
%% @end
%%-----------------------------------------------------------------------------
-spec join(AgentId :: models:agent_id()) -> boolean().
join(AgentId) ->
  RootNode = get_root(),
  BB = RootNode#quad_node.bb,
  {X, Y} = generate_xy(BB#bounding_box.width, BB#bounding_box.height),
  XYP = build_xy_point(X, Y, AgentId),
  do_insert(XYP, RootNode).

%%-----------------------------------------------------------------------------
%% @private
%% @doc remove Remove point from tree based on AgentId.
%% @end
%%-----------------------------------------------------------------------------
-spec remove(AgentId :: models:agent_id()) -> boolean().
remove(AgentId) ->
  [{AgentId, {_Pid, {X, Y}}}] = ets:lookup(ids_sids_loc, AgentId),
  remove(X, Y, AgentId).

%%-----------------------------------------------------------------------------
%% @private
%% @doc remove Remove point at X,Y from tree.
%% @end
%%-----------------------------------------------------------------------------
-spec remove(X :: float(), Y :: float(), AgentId :: models:agent_id()) -> boolean().
remove(X, Y, AgentId) ->
  XYP = build_xy_point(X, Y, AgentId),
  RootNode = get_root(),
  do_remove(XYP, RootNode).

%%-----------------------------------------------------------------------------
%% @private
%% @doc query_range Range query of the tree.
%% @end
%%-----------------------------------------------------------------------------
-spec query_range(X :: float(), Y :: float(), W :: float(), H :: float()) -> list().
query_range(X, Y, Width, Height) ->
  XYP = build_xy_point(X, Y),
  RootNode = get_root(),
  Range = build_bounding_box(XYP, Width, Height),
  do_query_range(Range, RootNode).

%%-----------------------------------------------------------------------------
%% @private
%% @doc fetch_sector Retrieves the SectorUId from the ETS based on the AgentId.
%% @end
%%-----------------------------------------------------------------------------
-spec fetch_sector(AgentId :: models:agent_id()) -> integer().
fetch_sector(AgentId) ->
  [{AgentId, {SectorUId, _Loc}}] = ets:lookup(ids_sids_loc, AgentId),
  SectorUId.

%%-----------------------------------------------------------------------------
%% @private
%% @doc stop_sectors Sends a stop message to all the sector processes that
%%      compose this scape.
%% @end
%%-----------------------------------------------------------------------------
-spec stop_sectors() -> ok.
stop_sectors() ->
  ets:foldl(fun({_Id, {UId, _Loc}}, ok) ->
    exit(UId, terminate), ok end, ok, ids_sids_loc).

%%%============================================================================
%%% QT Internal functions
%%%============================================================================

-spec do_insert(XYPoint :: xy_point(), quad_node()) -> boolean().
do_insert(XYP, QN) ->
  % Ignore objects which do not belong in this quad node
  case bounding_box_contains_point(XYP, QN#quad_node.bb) of
    true ->
      case (is_leaf(QN) and (lists:member(XYP, QN#quad_node.points))) of
        true ->
          false;
        false ->
          case (is_leaf(QN) and (length(QN#quad_node.points) < ?MAX_CAPACITY)) of
              true ->
                %% If there is space in this quad node, add the object here
                UQN = QN#quad_node{points = lists:append([XYP], QN#quad_node.points)},
                true = ets:insert(ids_sids_loc, {XYP#xy_point.agent_id,
                  {UQN#quad_node.uid, XYP#xy_point.point}}),
                true = ets:insert(xy_pts, {XYP#xy_point.point, XYP}),
                true = ets:insert(qt, {UQN#quad_node.uid, UQN});
              false ->
                case (is_leaf(QN) and (length(QN#quad_node.points) >= ?MAX_CAPACITY)) of
                  true ->
                    % We need to subdivide then add the point to whichever node will accept it
                    UQN = subdivide(QN),
                    insert_into_children([XYP], UQN);
                  false ->
                    insert_into_children([XYP], QN)
                end
          end
      end;
    false ->
      false
  end.

-spec do_remove(XYPoint :: xy_point(), QuadNode :: quad_node()) -> boolean().
do_remove(XYP, QN) ->
  case bounding_box_contains_point(XYP, QN#quad_node.bb) of
    true -> % If in this BB and in this node
      case lists:member(XYP, QN#quad_node.points) of
        true ->
          UQN = QN#quad_node{points = lists:delete(XYP, QN#quad_node.points)},
          true = ets:delete(ids_sids_loc, XYP#xy_point.agent_id),
          true = ets:delete(xy_pts, XYP#xy_point.point),
          true = ets:insert(qt, {UQN#quad_node.uid, UQN});
        false ->
          case is_leaf(QN) of % If this node has children
            true ->
              false;
            false ->
              case remove_from_children(XYP, QN) of % If in this BB but in a child branch
                true ->
                  merge(QN),
                  true;
                false ->
                  false
              end
          end
      end;
    false -> % If not in this BB, don't do anything
      false
  end.

-spec do_query_range(Range :: bounding_box(), quad_node()) -> PtsInRange :: [xy_point()]|undefined.
do_query_range(Range, QN) ->
  % Automatically abort if the range does not collide with this quad
  case intersects_box(QN#quad_node.bb, Range) of
    true ->
      % If leaf, check objects at this level
      case is_leaf(QN) of
        true ->
          [P || P <- QN#quad_node.points, bounding_box_contains_point(P, Range)];
        false ->
          % Otherwise, add the points from the children
          [{_NWUId, NW}] = ets:lookup(qt, QN#quad_node.north_west),
          [{_NEUId, NE}] = ets:lookup(qt, QN#quad_node.north_east),
          [{_SWUId, SW}] = ets:lookup(qt, QN#quad_node.south_west),
          [{_SEUId, SE}] = ets:lookup(qt, QN#quad_node.south_east),
          [do_query_range(Range, Child) || Child <- [NW, NE, SW, SE]]
      end;
    false ->
      undefined
  end.

-spec do_move(X :: float(), Y :: float(), AgentId :: models:agent_id()) -> boolean().
do_move(X, Y, AgentId) ->
 XYP = build_xy_point(X, Y, AgentId),
 RootNode = get_root(),
 do_remove(XYP, RootNode),
 do_insert(XYP, RootNode).

-spec do_whereis(AgentId :: models:agent_id()) -> {float(), float()} | not_found.
do_whereis(AgentId) ->
  case ets:lookup(ids_sids_loc, AgentId) of
    [] ->
      not_found;
    [{AgentId, {_Pid, {X, Y}}}] ->
      {X, Y}
  end.

-spec subdivide(QuadNode :: quad_node()) -> quad_node().
subdivide(QN) ->
  BB = QN#quad_node.bb,
  ModName = QN#quad_node.mod_name,
  XYPoints = QN#quad_node.points,
  H = QN#quad_node.bb#bounding_box.height / 2,
  W = QN#quad_node.bb#bounding_box.width / 2,
  % NW
  XYNW = build_xy_point(BB#bounding_box.x, BB#bounding_box.y),
  BBNW = build_bounding_box(XYNW, W, H),
  NW = build_quad_node(BBNW, ModName),
  % NE
  XYNE = build_xy_point(BB#bounding_box.x + W, BB#bounding_box.y),
  BBNE = build_bounding_box(XYNE, W, H),
  NE = build_quad_node(BBNE, ModName),
  % SW
  XYSW = build_xy_point(BB#bounding_box.x, BB#bounding_box.y + H),
  BBSW = build_bounding_box(XYSW, W, H),
  SW = build_quad_node(BBSW, ModName),
  % SE
  XYSE = build_xy_point(BB#bounding_box.x + W, BB#bounding_box.y + H),
  BBSE = build_bounding_box(XYSE, W, H),
  SE = build_quad_node(BBSE, ModName),
  % Points live in leaf nodes, so distribute
  UQN = QN#quad_node{
    north_west = NW#quad_node.uid,
    north_east = NE#quad_node.uid,
    south_west = SW#quad_node.uid,
    south_east = SE#quad_node.uid,
    height = QN#quad_node.height + 4,
    points = []
  },
  true = ets:insert(qt, [
    {NW#quad_node.uid, NW},
    {NE#quad_node.uid, NE},
    {SW#quad_node.uid, SW},
    {SE#quad_node.uid, SE},
    {UQN#quad_node.uid, UQN}
  ]),
  insert_into_children(XYPoints, UQN),
  UQN.

-spec insert_into_children([XYPoint :: xy_point()] | [], QuadNode :: quad_node()) -> boolean().
insert_into_children([XYP | XYPoints], QN) ->
  [{_NWUId, NW}] = ets:lookup(qt, QN#quad_node.north_west),
  [{_NEUId, NE}] = ets:lookup(qt, QN#quad_node.north_east),
  [{_SWUId, SW}] = ets:lookup(qt, QN#quad_node.south_west),
  [{_SEUId, SE}] = ets:lookup(qt, QN#quad_node.south_east),
  % A point can only live in one child
  case do_insert(XYP, NW) of
    true ->
      redistribute([XYP], QN#quad_node.uid, NW#quad_node.uid),
      insert_into_children(XYPoints, QN);
    false ->
      case do_insert(XYP, NE) of
        true ->
          redistribute([XYP], QN#quad_node.uid, NE#quad_node.uid),
          insert_into_children(XYPoints, QN);
        false ->
          case do_insert(XYP, SW) of
            true ->
              redistribute([XYP], QN#quad_node.uid, SW#quad_node.uid),
              insert_into_children(XYPoints, QN);
            false ->
              case do_insert(XYP, SE) of
                true ->
                  redistribute([XYP], QN#quad_node.uid, SE#quad_node.uid),
                  insert_into_children(XYPoints, QN)
              end
          end
      end
  end;
insert_into_children([], _QN) ->
  true.

-spec redistribute([XYP :: xy_point()] | [], OldUId :: integer(), NewUId :: integer()) -> boolean().
redistribute([XYP | XYPoints], OldUId, NewUId) ->
  Agent = sector:remove(OldUId, XYP#xy_point.agent_id),
  sector:insert(NewUId, XYP#xy_point.agent_id, Agent),
  redistribute(XYPoints, OldUId, NewUId);
redistribute([], _OldUId, _NewUId) ->
  true.

-spec remove_from_children(XYPoint :: xy_point(), QuadNode :: quad_node()) -> boolean().
remove_from_children(XYP, QN) ->
  [{_NWUId, NW}] = ets:lookup(qt, QN#quad_node.north_west),
  [{_NEUId, NE}] = ets:lookup(qt, QN#quad_node.north_east),
  [{_SWUId, SW}] = ets:lookup(qt, QN#quad_node.south_west),
  [{_SEUId, SE}] = ets:lookup(qt, QN#quad_node.south_east),
  % A point can only live in one child
  case do_remove(XYP, NW) of
    true ->
      true;
    false ->
      case do_remove(XYP, NE) of
        true ->
          true;
        false ->
          case do_remove(XYP, SW) of
            true ->
              true;
            false ->
              case do_remove(XYP, SE) of
                true ->
                  true;
                false ->
                  false
              end
          end
      end
  end.

-spec merge(QuadNode :: quad_node()) -> boolean().
merge(QN) ->
  [{_NWUId, NW}] = ets:lookup(qt, QN#quad_node.north_west),
  [{_NEUId, NE}] = ets:lookup(qt, QN#quad_node.north_east),
  [{_SWUId, SW}] = ets:lookup(qt, QN#quad_node.south_west),
  [{_SEUId, SE}] = ets:lookup(qt, QN#quad_node.south_east),
  % If the children aren't leafs, you cannot merge
  case is_leaf(NW) and is_leaf(NE) and is_leaf(SW) and is_leaf(SE) of
    true ->
      % Children are leafs, see if you can remove point and merge into this node
      TotalSizeChildren = (length(NW#quad_node.points) + length(NE#quad_node.points) +
        length(SW#quad_node.points) + length(SE#quad_node.points)),
      TotalSizeParent = length(QN#quad_node.points),
      % If all the children's points can be merged into this node
      case ((TotalSizeParent + TotalSizeChildren) < ?MAX_CAPACITY) of
        true ->
          ListOfLists = [
            lists:sort(QN#quad_node.points),
            lists:sort(NW#quad_node.points),
            lists:sort(NE#quad_node.points),
            lists:sort(SW#quad_node.points),
            lists:sort(SE#quad_node.points)
          ],
          UQN = QN#quad_node{
            north_west = undefined,
            north_east = undefined,
            south_west = undefined,
            south_east = undefined,
            points = lists:merge(lists:sort(ListOfLists))
          },
          true = ets:insert(qt, {UQN#quad_node.uid, UQN}),

          true = ets:delete(qt, {NW#quad_node.uid, NW}),
          redistribute(NW#quad_node.points, QN#quad_node.uid, NW#quad_node.uid),
          true = ets:delete(qt, {NE#quad_node.uid, NE}),
          redistribute(NE#quad_node.points, QN#quad_node.uid, NE#quad_node.uid),
          true = ets:delete(qt, {SW#quad_node.uid, SW}),
          redistribute(SW#quad_node.points, QN#quad_node.uid, SW#quad_node.uid),
          true = ets:delete(qt, {SE#quad_node.uid, SE}),
          redistribute(SE#quad_node.points, QN#quad_node.uid, SE#quad_node.uid);
        false ->
          false
      end;
    false ->
      false
  end.

-spec build_xy_point(X :: float(), Y :: float()) -> xy_point().
build_xy_point(X, Y) ->
  XYP = #xy_point{
    x = X,
    y = Y,
    point = {X, Y}
  },
  XYP.

-spec build_xy_point(X :: float(), Y :: float(), AgentId :: models:agent_id()) -> xy_point().
build_xy_point(X, Y, AgentId) ->
  XYP = #xy_point{
    x = X,
    y = Y,
    point = {X, Y},
    agent_id = AgentId
  },
  XYP.

-spec build_bounding_box(UpperLeft :: xy_point(), W :: float(), H :: float()) -> bounding_box().
build_bounding_box(UpperLeft, Width, Height) ->
  BB = #bounding_box{
    x = UpperLeft#xy_point.x,
    y = UpperLeft#xy_point.y,
    width = Width,
    height = Height,
    min_x = UpperLeft#xy_point.x,
    min_y = UpperLeft#xy_point.y,
    max_x = UpperLeft#xy_point.x + Width,
    max_y = UpperLeft#xy_point.y + Height
  },
  BB.

-spec build_quad_node(BB :: bounding_box(), ModName :: atom()) -> quad_node().
build_quad_node(BB, ModName) ->
  UId = erlang:unique_integer([positive, monotonic]),
  {ok, _Pid} = sector_sup:start_sector(ModName, UId),
  QN = #quad_node{
    uid = UId,
    bb = BB,
    height = 1,
    mod_name = ModName
  },
  QN.

-spec build_root_quad_node(BB :: bounding_box(), ModName :: atom()) -> quad_node().
build_root_quad_node(BB, ModName) ->
  UId = root,
  {ok, _Pid} = sector_sup:start_sector(ModName, UId),
  QN = #quad_node{
    uid = UId,
    bb = BB,
    height = 1,
    mod_name = ModName
  },
  QN.

-spec is_leaf(quad_node()) -> boolean().
is_leaf(QN) ->
  ((QN#quad_node.north_west == undefined)
    and (QN#quad_node.north_east == undefined)
    and (QN#quad_node.south_west == undefined)
    and (QN#quad_node.south_east == undefined)).

-spec bounding_box_contains_point(XYPoint :: xy_point(), B :: bounding_box()) -> boolean().
bounding_box_contains_point(P, B) when (P#xy_point.x >= B#bounding_box.max_x) ->
  false;
bounding_box_contains_point(P, B) when (P#xy_point.x < B#bounding_box.min_x) ->
  false;
bounding_box_contains_point(P, B) when (P#xy_point.y >= B#bounding_box.max_y) ->
  false;
bounding_box_contains_point(P, B) when (P#xy_point.y < B#bounding_box.min_y) ->
  false;
bounding_box_contains_point(_P, _B) ->
  true.

-spec inside_this_box(Box :: bounding_box(), OtherBox :: bounding_box()) -> boolean().
inside_this_box(Box, OtherBox) ->
  ((OtherBox#bounding_box.min_x >= Box#bounding_box.min_x)
    and (OtherBox#bounding_box.max_x =< Box#bounding_box.max_x)
    and (OtherBox#bounding_box.min_y >= Box#bounding_box.min_y)
    and (OtherBox#bounding_box.max_y =< Box#bounding_box.max_y)).

-spec intersects_box(Box :: bounding_box(), OtherBox :: bounding_box()) -> boolean().
intersects_box(Box, OtherBox) ->
  case (inside_this_box(Box, OtherBox) or inside_this_box(OtherBox, Box)) of % Inside
    true ->
      true;
    false ->
      case (Box#bounding_box.max_x < OtherBox#bounding_box.min_x) or
        (Box#bounding_box.min_x > OtherBox#bounding_box.max_x) of % Outside X
          true ->
            false;
          false ->
            case (Box#bounding_box.max_y < OtherBox#bounding_box.min_y) or
              (Box#bounding_box.min_y > OtherBox#bounding_box.max_y) of % Outside Y
                true ->
                  false;
                false -> % Intersects
                  true
            end
      end
  end.

-spec generate_xy(X :: float(), Y :: float()) -> {float(), float()}.
generate_xy(X, Y) ->
  {Xx, Yy} = {rand:uniform(round(X)) / 1, rand:uniform(round(Y)) / 1},
  case ets:member(xy_pts, {Xx, Yy}) of
    true ->
      generate_xy(X, Y);
    false ->
      {Xx, Yy}
  end.