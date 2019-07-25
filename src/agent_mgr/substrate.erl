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
%%% @doc In indirect encoded NN systems, the genotype and phenotype do not
%%%      have a 1-to-1 mapping. Substrate Encoding is one of such indirect
%%%      encoding method. It has numerous advantages, particularly when it
%%%      comes to generalization, image analysis based problems, and problems
%%%      with geometrical regularities. Substrate encoding allows us to build
%%%      substrate modules whose embedded interconnected neurodes and their
%%%      very connections and synaptic weights are defined by a directly
%%%      encoded NN system. Thus by evolving a NN, we are evolving a system
%%%      which accepts as input the coordinates of the substrate embedded
%%%      neurodes and outputs the topology, synaptic weights, connection
%%%      expression patterns, and other connectivity parameters on the
%%%      multidimensional substrate.
%%% @end
%%%----------------------------------------------------------------------------
-module(substrate).

%% Start/Stop
-export([
  start/2,
  stop/2
]).

%% API
-export([
  init_phase2/11,
  forward/3,
  reset_substrate/2,
  backup_substrate/2,
  revert_substrate/2,
  set_weight/3,
  weight_expression/3,
  set_abcn/3,
  set_iterative/3,
  abcn/4
]).

%% Callbacks
-export([
  init/1,
  loop/1, loop/4,
  handle/2,
  terminate/1
]).

%% Xref
-ignore_xref([
  loop/1, loop/4,
  handle/2,
  forward/3,
  reset_substrate/2,
  backup_substrate/2,
  revert_substrate/2,
  set_weight/3,
  weight_expression/3,
  set_abcn/3,
  set_iterative/3,
  abcn/4,
  terminate/1
]).

%%%============================================================================
%%% Types
%%%============================================================================

-record(sub_state, {
  type,
  plasticity = none :: none | iterative | abcn,
  morphology :: atom(),
  sensors :: [models:sensor()],
  actuators :: [models:actuator()],
  spids = [] :: [pid()],
  apids = [] :: [pid()],
  cpp_pids = [] :: [pid()],
  cep_pids = [] :: [pid()],
  densities :: [integer()],
  substrate_state_flag :: reset | iterative | hold,
  old_substrate :: init | void | [any()],
  cur_substrate :: init | void | [any()],
  linkform :: models:linkform()
}).

-type state() :: #sub_state{}.

%%%============================================================================
%%% API
%%%============================================================================

%%-----------------------------------------------------------------------------
%% @doc Spawns a Substrate process belonging to the Exoself process that
%%      spawned it and calls init to initialize.
%% @end
%%-----------------------------------------------------------------------------
-spec start(node(), pid()) -> pid().
start(Node, ExoselfPid) ->
  spawn_link(Node, ?MODULE, init, [ExoselfPid]).

%%-----------------------------------------------------------------------------
%% @doc Terminates substrate.
%% @end
%%-----------------------------------------------------------------------------
-spec stop(pid(), pid()) -> ok.
stop(Pid, ExoselfPid) ->
  Pid ! {ExoselfPid, stop},
  ok.

%%-----------------------------------------------------------------------------
%% @doc Initializes substrate.
%% @end
%%-----------------------------------------------------------------------------
-spec init_phase2(pid(), pid(), [models:sensor()], [models:actuator()], [pid()],
  [pid()], [pid()], [pid()], [integer()], none | iterative | abcn, models:linkform()) -> ok.
init_phase2(Pid, ExoselfPid, Sensors, Actuators, SPids, APids, CPPPids,
  CEPPids, Densities, Plasticity, LinkForm) ->
  Pid ! {handle, {init_phase2, ExoselfPid, Sensors, Actuators, SPids, APids, CPPPids,
  CEPPids, Densities, Plasticity, LinkForm}},
  ok.

%%-----------------------------------------------------------------------------
%% @doc Sensors poll the environment for sensory signals. Sensors do
%%      postprocessing of the signals. Sensors forward the processed sensory
%%      signals to the substrate. The substrate process gathers all the signals
%%      from the sensors, and based on those signals, its densities, and the
%%      actuators, constructs a substrate if its substrate_state_flag is set
%%      to reset. If substrate_state_flag is set to hold, the substrate sends
%%      the coordinates of the connected neurodes to the substrate_cpps it is
%%      connected to.
%% @end
%%-----------------------------------------------------------------------------
-spec forward(pid(), pid(), float()) -> ok.
forward(Pid, SPid, Input) ->
  Pid ! {handle, {forward, SPid, Input}},
  ok.

%%-----------------------------------------------------------------------------
%% @doc Exoself sends a message to reset the substrate when the exoself
%%      perturbs the NN.
%% @end
%%-----------------------------------------------------------------------------
-spec reset_substrate(pid(), pid()) -> ok.
reset_substrate(Pid, ExoselfPid) ->
  Pid ! {handle, {ExoselfPid, reset_substrate}},
  ok.

%%-----------------------------------------------------------------------------
%% @doc Backup substrate.
%% @end
%%-----------------------------------------------------------------------------
-spec backup_substrate(pid(), pid()) -> ok.
backup_substrate(Pid, ExoselfPid) ->
  Pid ! {handle, {ExoselfPid, backup_substrate}},
  ok.

%%-----------------------------------------------------------------------------
%% @doc Revert substrate.
%% @end
%%-----------------------------------------------------------------------------
-spec revert_substrate(pid(), pid()) -> ok.
revert_substrate(Pid, ExoselfPid) ->
  Pid ! {handle, {ExoselfPid, revert_substrate}},
  ok.

%%-----------------------------------------------------------------------------
%% @doc set_weight substrate actuator.
%% @end
%%-----------------------------------------------------------------------------
-spec set_weight(pid(), pid(), [float()]) -> ok.
set_weight(Pid, CEPPid, Signal) ->
  Pid ! {handle, {CEPPid, set_weight, Signal}},
  ok.

%%-----------------------------------------------------------------------------
%% @doc weight_expression substrate actuator.
%% @end
%%-----------------------------------------------------------------------------
-spec weight_expression(pid(), pid(), [float()]) -> ok.
weight_expression(Pid, CEPPid, Signal) ->
  Pid ! {handle, {CEPPid, weight_expression, Signal}},
  ok.

%%-----------------------------------------------------------------------------
%% @doc set_abcn substrate actuator.
%% @end
%%-----------------------------------------------------------------------------
-spec set_abcn(pid(), pid(), [float()]) -> ok.
set_abcn(Pid, CEPPid, Signal) ->
  Pid ! {handle, {CEPPid, set_abcn, Signal}},
  ok.

%%-----------------------------------------------------------------------------
%% @doc set_iterative substrate actuator.
%% @end
%%-----------------------------------------------------------------------------
-spec set_iterative(pid(), pid(), [float()]) -> ok.
set_iterative(Pid, CEPPid, Signal) ->
  Pid ! {handle, {CEPPid, set_iterative, Signal}},
  ok.

%%%============================================================================
%%% Callbacks
%%%============================================================================

%%-----------------------------------------------------------------------------
%% @private
%% @doc Whenever a Substrate process is started via the start function this
%%      function is called by the new process to initialize.
%% @end
%%-----------------------------------------------------------------------------
-spec init(pid()) -> no_return().
init(ExoselfPid) ->
  utils:random_seed(),
  logr:debug({substrate, init, ok, undefined, []}),
  loop(ExoselfPid).

%%-----------------------------------------------------------------------------
%% @private
%% @doc Receive and handle messages.
%% @end
%%-----------------------------------------------------------------------------
-spec loop(pid()) -> no_return().
loop(ExoselfPid) ->
  receive
    {handle, {init_phase2, ExoselfPid, Sensors, Actuators, SPids, APids, CPPPids,
    CEPPids, Densities, Plasticity, LinkForm}} ->
    NewState = handle(init_phase2, {ExoselfPid, Sensors, Actuators,
      SPids, APids, CPPPids, CEPPids, Densities, Plasticity, LinkForm}),
    loop(NewState, ExoselfPid, SPids, [])
  end.

%%-----------------------------------------------------------------------------
%% @private
%% @doc Receive and handle messages.
%% @end
%%-----------------------------------------------------------------------------
-spec loop(state(), pid(), [pid()] | [], [float()]) -> no_return().
loop(S, ExoselfPid, [SPid | SPids], SAcc) ->
  receive
    {handle, {forward, SPid, SensorySignal}} ->
      logr:debug({substrate, msg, ok, "SPid forward message received", [SPid]}),
      loop(S, ExoselfPid, SPids, [SensorySignal | SAcc]);
    {handle, {ExoselfPid, reset_substrate}} ->
      US = S#sub_state{old_substrate = S#sub_state.cur_substrate, substrate_state_flag = reset},
      logr:debug({substrate, msg, ok, "reset_substrate", []}),
      ExoselfPid ! {self(), ready},
      loop(US, ExoselfPid, [SPid | SPids], SAcc);
    {handle, {ExoselfPid, backup_substrate}} ->
      US = S#sub_state{old_substrate = S#sub_state.cur_substrate, substrate_state_flag = reset},
      logr:debug({substrate, msg, ok, "backup_substrate", []}),
      ExoselfPid ! {self(), ready},
      loop(US, ExoselfPid, [SPid | SPids], SAcc);
    {handle, {ExoselfPid, revert_substrate}} ->
      US = S#sub_state{cur_substrate = S#sub_state.old_substrate, substrate_state_flag = reset},
      logr:debug({substrate, msg, ok, "revert_substrate", []}),
      ExoselfPid ! {self(), ready},
      loop(US, ExoselfPid, [SPid | SPids], SAcc);
    {ExoselfPid, stop} ->
      terminate(normal)
    after 300000 ->
      logr:warning({substrate, msg, error, "messages not received", [SPid]}),
      exit(substrate_timeout)
  end;

loop(S, ExoselfPid, [], SAcc) ->
  {USubstrate, USMode, OAcc} = reason(SAcc, S),
  logr:debug({substrate, msg, ok, "all SPids received. fanning out", []}),
  advanced_fanout(OAcc, S#sub_state.actuators, S#sub_state.apids),
  US = S#sub_state{cur_substrate = USubstrate, substrate_state_flag = USMode},
  loop(US, ExoselfPid, S#sub_state.spids, []).

%%-----------------------------------------------------------------------------
%% @private
%% @doc This function is called to terminate the process. It performs
%%      any necessary cleaning up before exiting with the << Reason >>
%%      parameter that it was called with.
%% @end
%%-----------------------------------------------------------------------------
-spec terminate(atom()) -> ok.
terminate(Reason) ->
  logr:debug({substrate, terminate, ok, undefined, [Reason]}),
  exit(Reason).

%%%============================================================================
%%% Internal functions
%%%============================================================================

handle(init_phase2, {_ExoselfPid, Sensors, Actuators, SPids, APids, CPPPids,
  CEPPids, Densities, Plasticity, LinkForm}) ->
  logr:debug({substrate, init2, ok, undefined, []}),
  #sub_state{
    sensors = Sensors,
    actuators = Actuators,
    spids = SPids,
    apids = APids,
    cpp_pids = CPPPids,
    cep_pids = CEPPids,
    densities = Densities,
    substrate_state_flag = reset,
    old_substrate = void,
    cur_substrate = init,
    plasticity = Plasticity,
    linkform = LinkForm
  };

handle(set_weight, {Signal, _W}) ->
  [UW] = Signal,
  functions:sat(UW, 3.1415, -3.1415);

handle(weight_expression, {Signal, _W}) ->
  [UW, Expression] = Signal,
  case Expression > 0 of
    true ->
      functions:sat(UW, 3.1415, -3.1415);
    false ->
      0
  end;

handle(set_abcn, {Signal, _W}) ->
  [UW, A, B, C, N] = Signal,
  {functions:sat(UW, 3.1415, -3.1415), abcn, [A, B, C, N]};

handle(set_iterative, {Signal, W}) ->
  [DeltaWeight] = Signal,
  functions:sat(W + DeltaWeight, 3.1415, -3.1415).

reason(Input, S) ->
  Densities = S#sub_state.densities,
  Substrate = S#sub_state.cur_substrate,
  SMode = S#sub_state.substrate_state_flag,
  CPPPids = S#sub_state.cpp_pids,
  CEPPids = S#sub_state.cep_pids,
  Plasticity = S#sub_state.plasticity,
  s_mode(Input, S, Densities, Substrate, SMode, CPPPids, CEPPids, Plasticity).

s_mode(Input, S, Densities, Substrate, SMode, CPPPids, CEPPids, Plasticity) ->
  case SMode of
    reset ->
      Sensors = S#sub_state.sensors,
      Actuators = S#sub_state.actuators,
      NewSubstrate = create_substrate(Sensors, Densities, Actuators, S#sub_state.linkform),
      {Output, PopulatedSubstrate} = calculate_reset_output(Densities, NewSubstrate, Input,
        CPPPids, CEPPids, Plasticity, S#sub_state.linkform),
      USMode = case Plasticity of
        iterative ->
          iterative;
        _ ->
          hold
      end,
      {PopulatedSubstrate, USMode, Output};
    iterative ->
      {Output, USubstrate} = calculate_reset_output(Densities, Substrate, Input, CPPPids, CEPPids,
        Plasticity, S#sub_state.linkform),
      {USubstrate, SMode, Output};
    hold ->
      {Output, USubstrate} = calculate_hold_output(Densities, Substrate, Input,
        S#sub_state.linkform, Plasticity, CPPPids, CEPPids),
      {USubstrate, SMode, Output}
  end.

advanced_fanout(OAcc, [Actuator | Actuators], [APid | APids]) ->
  {Output, OAccRem} = lists:split(models:get(vl, Actuator), OAcc),
  APid ! {forward, self(), Output},
  advanced_fanout(OAccRem, Actuators, APids);
advanced_fanout([], [], []) ->
  ok.

create_substrate(Sensors, Densities, Actuators, LinkForm) ->
  [Depth | SubDensities] = Densities,
  SubstrateI = compose_isubstrate(Sensors, length(Densities)),
  IVL = length(SubstrateI),
  {IWeights, HWeights} = case LinkForm of
    l2l_feedforward ->
      Weight = 0,
      H = mult(SubDensities),
      {lists:duplicate(IVL, Weight), lists:duplicate(H, Weight)};
    fully_interconnected ->
      OutputNeurodes = tot_oneurodes(Actuators, 0),
      Weight = 0,
      TotHiddenNeurodes = mult([Depth - 1 | SubDensities]),
      TotWeights = TotHiddenNeurodes + IVL + OutputNeurodes,
      {lists:duplicate(TotWeights, Weight), lists:duplicate(TotWeights, Weight)};
    jordan_recurrent ->
      OutputNeurodes = tot_oneurodes(Actuators, 0),
      Weight = 0,
      H = mult(SubDensities),
      {lists:duplicate(IVL + OutputNeurodes, Weight), lists:duplicate(H, Weight)};
    neuronself_recurrent ->
      Weight = 0,
      H = mult(SubDensities),
      {lists:duplicate(IVL + 1, Weight), lists:duplicate(H + 1, Weight)}
  end,
  case Depth of
    0 ->
      SubstrateO = compose_osubstrate(Actuators, length(Densities), IWeights),
      [SubstrateI, SubstrateO];
    1 ->
      SubstrateR = cs(SubDensities, IWeights),
      SubstrateO = compose_osubstrate(Actuators, length(Densities), HWeights),
      [SubstrateI, extrude(0, SubstrateR), SubstrateO];
    _ ->
      SubstrateR = cs(SubDensities, IWeights),
      SubstrateH = cs(SubDensities, HWeights),
      SubstrateO = compose_osubstrate(Actuators, length(Densities), HWeights),
      [_, RCoord|C1] = build_coord_list(Depth + 1),
      [_ | C2] = lists:reverse(C1),
      HCoords = lists:reverse(C2),
      ESubstrateR = extrude(RCoord, SubstrateR),
      ESubstratesH = [extrude(HCoord, SubstrateH) || HCoord <- HCoords],
      lists:append([[SubstrateI, ESubstrateR], ESubstratesH, [SubstrateO]])
  end.

compose_isubstrate(Sensors, SubstrateDimension) ->
  compose_isubstrate(Sensors, [], 1, SubstrateDimension -2).

compose_isubstrate([S | Sensors], Acc, MaxDim, RequiredDim) ->
  {Dim, ISubstratePart} = case models:get(format, S) of
    undefined ->
      CoordLists = create_coord_lists([models:get(vl, S)]),
      ISubPart = [{Coord, 0, void} || Coord <- CoordLists],
      {1, ISubPart};
    no_geo ->
      CoordLists = create_coord_lists([models:get(vl, S)]),
      ISubPart = [{Coord, 0, void} || Coord <- CoordLists],
      {1, ISubPart};
    {symmetric, Resolutions} ->
      _SignalLength = mult(Resolutions),
      CoordLists = create_coord_lists(Resolutions),
      ISubPart = [{Coord, 0, void} || Coord <- CoordLists],
      {length(Resolutions), ISubPart};
    {coorded, D, _Resolutions, ISubPart} ->
      {D, ISubPart}
  end,
  UDim = case MaxDim > Dim of
    true ->
      MaxDim;
    false ->
      Dim
  end,
  compose_isubstrate(Sensors, [ISubstratePart | Acc], UDim, RequiredDim);
compose_isubstrate([], Acc, ISubstratePartMaxDim, RequiredDim) ->
  case RequiredDim >= ISubstratePartMaxDim of
    true ->
      ISubstrateDepth = length(Acc),
      ISubstrateDepthCoords = build_coord_list(ISubstrateDepth),
      adv_extrude(Acc, RequiredDim, lists:reverse(ISubstrateDepthCoords), -1, []);
    false ->
      exit("Error in adv_extrude, Required_Depth < ISubstratePart_MaxDepth~n")
  end.

adv_extrude([ISubstratePart | ISubstrate], RequiredDim,
  [IDepthCoord | ISubstrateDepthCoords], LeadCoord, Acc) ->
  ExtrudedISP = [
    {[LeadCoord,
      IDepthCoord | lists:append(lists:duplicate(RequiredDim - length(Coord), 0), Coord)],
      O, W} || {Coord, O, W} <- ISubstratePart],
  extrude(ISubstratePart, RequiredDim, IDepthCoord, []),
  adv_extrude(ISubstrate, RequiredDim, ISubstrateDepthCoords,
    LeadCoord, lists:append(ExtrudedISP, Acc));
adv_extrude([], _RequiredDim, [], _LeadCoord, Acc) ->
  Acc.

extrude([{Coord, O, W} | ISubstratePart], RequiredDim, DepthCoord, Acc) ->
  DimDif = RequiredDim - length(Coord),
  UCoord = [1, DepthCoord | lists:append(lists:duplicate(DimDif, 0), Coord)],
  extrude(ISubstratePart, RequiredDim, DepthCoord, [{UCoord, O, W} | Acc]);
extrude([], _RequiredDim, _DepthCoord, Acc) ->
  Acc.

compose_osubstrate(Actuators, SubstrateDimension, Weights) ->
  compose_osubstrate(Actuators, [], 1, SubstrateDimension -2, Weights).

compose_osubstrate([A | Actuators], Acc, MaxDim, RequiredDim, Weights) ->
  {Dim, OSubstratePart} = case models:get(format, A) of
    undefined ->
      CoordLists = create_coord_lists([models:get(vl, A)]),
      OSubPart = [{Coord, 0, Weights} || Coord <- CoordLists],
      {1, OSubPart};
    no_geo ->
      CoordLists = create_coord_lists([models:get(vl, A)]),
      OSubPart = [{Coord, 0, Weights} || Coord <- CoordLists],
      {1, OSubPart};
    {symmetric, Resolutions} ->
      _SignalLength = mult(Resolutions),
      CoordLists = create_coord_lists(Resolutions),
      OSubPart = [{Coord, 0, Weights} || Coord <- CoordLists],
      {length(Resolutions), OSubPart};
    {coorded, D, _Resolutions, UnadjustedOSubPart} ->
      OSubPart = [{Coord, O, Weights} || {Coord, O, _} <- UnadjustedOSubPart],
      {D, OSubPart}
  end,
  UDim = case MaxDim > Dim of
    true ->
      MaxDim;
    false ->
      Dim
  end,
  compose_osubstrate(Actuators, [OSubstratePart | Acc], UDim, RequiredDim, Weights);
compose_osubstrate([], Acc, OSubstratePartMaxDim, RequiredDim, _Weights) ->
  case RequiredDim >= OSubstratePartMaxDim of
    true ->
      ISubstrateDepth = length(Acc),
      ISubstrateDepthCoords = build_coord_list(ISubstrateDepth),
      adv_extrude(Acc, RequiredDim, lists:reverse(ISubstrateDepthCoords), 1, []);
    false ->
      exit("Error in adv_extrude, Required_Depth < OSubstratePart_MaxDepth~n")
  end.

build_coord_list(Density) ->
  case Density == 1 of
    true ->
      [0.0];
    false ->
      DensityDividers = Density - 1,
      Resolution = 2 / DensityDividers,
      build_coord_list(Resolution, DensityDividers, 1, [])
  end.

mult(List) ->
  mult(List, 1).

mult([Val | List], Acc) ->
  mult(List, Val * Acc);
mult([], Acc) ->
  Acc.

tot_oneurodes([A | Actuators], Acc) ->
  TotANeurodes = case models:get(format, A) of
    undefined ->
      models:get(vl, A);
    no_geo ->
      models:get(vl, A);
    {symmetric, Resolutions} ->
      mult(Resolutions);
    {coorded, _Dim, _Resolutions, UnadjustedOSubstratePart} ->
      length(UnadjustedOSubstratePart)
  end,
  tot_oneurodes(Actuators, TotANeurodes + Acc);
tot_oneurodes([], Acc) ->
  Acc.

cs(Densities, Weights) ->
  RDensities = lists:reverse(Densities),
  Substrate = create_coord_lists(RDensities, []),
  attach(Substrate, 0, Weights).

create_coord_lists(Densities) ->
  create_coord_lists(Densities, []).

create_coord_lists([Density | RDensities], []) ->
  CoordList = build_coord_list(Density),
  XtendedCoordList = [[Coord] || Coord <- CoordList],
  create_coord_lists(RDensities, XtendedCoordList);
create_coord_lists([Density | RDensities], Acc) ->
  CoordList = build_coord_list(Density),
  XtendedCoordList = [[Coord | SubCoord] || Coord <- CoordList, SubCoord <- Acc],
  create_coord_lists(RDensities, XtendedCoordList);
create_coord_lists([], Acc) ->
  Acc.

build_coord_list(_Resolution, 0, _Coord, Acc) ->
  [-1 | Acc];
build_coord_list(Resolution, DensityDividers, Coord, Acc) ->
  build_coord_list(Resolution, DensityDividers -1, Coord - Resolution, [Coord | Acc]).

attach(List, E1, E2) ->
  attach(List, E1, E2, []).

attach([Val|List], E1, E2, Acc) ->
  attach(List, E1, E2, [{Val, E1, E2} | Acc]);
attach([], _E1, _E2, Acc) ->
  lists:reverse(Acc).

extrude(NewDimensionCoord, Substrate) ->
  extrude(NewDimensionCoord, Substrate, []).

extrude(NewDimensionCoord, [{Coord, O, W} | Substrate], Acc) ->
  extrude(NewDimensionCoord, Substrate, [{[NewDimensionCoord | Coord], O, W} | Acc]);
extrude(_Coord, [], Acc) ->
  lists:reverse(Acc).

calculate_hold_output(_Densities, Substrate, Input, LinkForm, Plasticity, CPPPids, CEPPids) ->
  [IHyperlayer | PopulatedPHyperlayers] = Substrate,
  PopulatedIHyperlayer = populate_input_hyperlayer(IHyperlayer, lists:flatten(Input), []),
  {Output, UPHyperlayers} = calculate_substrate_output(PopulatedIHyperlayer, PopulatedPHyperlayers,
    LinkForm, Plasticity, CPPPids, CEPPids),
  {Output, [IHyperlayer | UPHyperlayers]}.

calculate_reset_output(_Densities, Substrate, Input, CPPPids, CEPPids, Plasticity, LinkForm) ->
  [IHyperlayer | PHyperlayers] = Substrate,
  PopulatedIHyperlayer = populate_input_hyperlayer(IHyperlayer, lists:flatten(Input), []),
  case Plasticity of
    iterative ->
      {Output, UPHyperlayers} = calculate_substrate_output(PopulatedIHyperlayer, PHyperlayers,
        LinkForm, Plasticity, CPPPids, CEPPids),
      {Output, [IHyperlayer | UPHyperlayers]};
    _->
      PopulatedPHyperlayers = populate_phyperlayers(Substrate, CPPPids, CEPPids,
        LinkForm, Plasticity),
      {Output, UPHyperlayers} = calculate_substrate_output(PopulatedIHyperlayer,
        PopulatedPHyperlayers, LinkForm, Plasticity, CPPPids, CEPPids),
      {Output, [IHyperlayer | UPHyperlayers]}
  end.

populate_input_hyperlayer([{Coord, _PrevO, void} | Substrate], [I | Input], Acc) ->
  populate_input_hyperlayer(Substrate, Input, [{Coord, I, void} | Acc]);
populate_input_hyperlayer([], [], Acc) ->
  lists:reverse(Acc).

populate_phyperlayers(Substrate, CPPPids, CEPPids, LinkForm, Plasticity) ->
  case LinkForm of
    l2l_feedforward ->
      [IHyperlayer, PHyperlayer | RemSubstrate] = Substrate,
      populate_phyperlayers_l2l(IHyperlayer, PHyperlayer, RemSubstrate,
        CPPPids, CEPPids, Plasticity, [], []);
    fully_interconnected ->
      [_IHyperlayer, PHyperlayer | RemSubstrate] = Substrate,
      INeurodes = lists:flatten(Substrate),
      populate_phyperlayers_fi(INeurodes, PHyperlayer, RemSubstrate,
        CPPPids, CEPPids, Plasticity, [], []);
    jordan_recurrent ->
      [IHyperlayer, PHyperlayer | RemSubstrate] = Substrate,
      [OHyperlayer | _] = lists:reverse(Substrate),
      INeurodes = lists:flatten([IHyperlayer, OHyperlayer]),
      populate_phyperlayers_l2l(INeurodes, PHyperlayer, RemSubstrate,
        CPPPids, CEPPids, Plasticity, [], []);
    neuronself_recurrent ->
      [IHyperlayer, PHyperlayer|RemSubstrate] = Substrate,
      populate_phyperlayers_nsr(IHyperlayer, PHyperlayer, RemSubstrate,
        CPPPids, CEPPids, Plasticity, [], [])
  end.

populate_phyperlayers_l2l(PrevHyperlayer, [{Coord, PrevO, PrevWeights} | CurHyperlayer],
  Substrate, CPPPids, CEPPids, Plasticity, Acc1, Acc2) ->
  NewWeights = new_weights(Plasticity, PrevHyperlayer, Coord, CPPPids, CEPPids, PrevWeights, PrevO),
  populate_phyperlayers_l2l(PrevHyperlayer, CurHyperlayer, Substrate, CPPPids, CEPPids, Plasticity,
    [{Coord, PrevO, NewWeights} | Acc1], Acc2);
populate_phyperlayers_l2l(_PrevHyperlayer, [], [CurHyperlayer | Substrate],
  CPPPids, CEPPids, Plasticity, Acc1, Acc2) ->
  PrevHyperlayer = lists:reverse(Acc1),
  populate_phyperlayers_l2l(PrevHyperlayer, CurHyperlayer, Substrate,
    CPPPids, CEPPids, Plasticity, [], [PrevHyperlayer | Acc2]);
populate_phyperlayers_l2l(_PrevHyperlayer, [], [], _CPPPids, _CEPPids, _Plasticity, Acc1, Acc2) ->
  lists:reverse([lists:reverse(Acc1) | Acc2]).

populate_phyperlayers_fi(FlatSubstrate, [{Coord, PrevO, PrevWeights} | CurHyperlayer],
  Substrate, CPPPids, CEPPids, Plasticity, Acc1, Acc2) ->
  NewWeights = new_weights(Plasticity, FlatSubstrate, Coord, CPPPids, CEPPids, PrevWeights, PrevO),
  populate_phyperlayers_fi(FlatSubstrate, CurHyperlayer, Substrate,
    CPPPids, CEPPids, Plasticity, [{Coord, PrevO, NewWeights} | Acc1], Acc2);
populate_phyperlayers_fi(FlatSubstrate, [], [CurHyperlayer | Substrate],
  CPPPids, CEPPids, Plasticity, Acc1, Acc2) ->
  populate_phyperlayers_fi(FlatSubstrate, CurHyperlayer, Substrate,
    CPPPids, CEPPids, Plasticity, [], [lists:reverse(Acc1) | Acc2]);
populate_phyperlayers_fi(_FlatSubstrate, [], [], _CPPPids, _CEPPids, _Plasticity, Acc1, Acc2) ->
  lists:reverse([lists:reverse(Acc1) | Acc2]).

populate_phyperlayers_nsr(PrevHyperlayer, [{Coord, PrevO, PrevWeights} | CurHyperlayer],
  Substrate, CPPPids, CEPPids, Plasticity, Acc1, Acc2) ->
  NewWeights = new_weights(Plasticity, [{Coord, PrevO, PrevWeights} | PrevHyperlayer],
    Coord, CPPPids, CEPPids, PrevWeights, PrevO),
  populate_phyperlayers_nsr(PrevHyperlayer, CurHyperlayer, Substrate,
    CPPPids, CEPPids, Plasticity, [{Coord, PrevO, NewWeights} | Acc1], Acc2);
populate_phyperlayers_nsr(_PrevHyperlayer, [], [CurHyperlayer | Substrate],
  CPPPids, CEPPids, Plasticity, Acc1, Acc2) ->
  PrevHyperlayer = lists:reverse(Acc1),
  populate_phyperlayers_nsr(PrevHyperlayer, CurHyperlayer, Substrate,
    CPPPids, CEPPids, Plasticity, [], [PrevHyperlayer | Acc2]);
populate_phyperlayers_nsr(_PrevHyperlayer, [], [], _CPPPids, _CEPPids, _Plasticity, Acc1, Acc2) ->
  lists:reverse([lists:reverse(Acc1)|Acc2]).

new_weights(Plasticity, Layer, Coord, CPPPids, CEPPids, PrevWeights, PrevO) ->
  case Plasticity of
    none ->
      get_weights(Layer, Coord, CPPPids, CEPPids, []);
    _ ->
      get_weights(Layer, Coord, CPPPids, CEPPids, [], PrevWeights, PrevO)
  end.

get_weights([{ICoord, _I, _IWeights} | INeurodes], Coord, CPPPids, CEPPids, Acc) ->
  static_fanout(CPPPids, ICoord, Coord),
  UW = fanin(CEPPids, void),
  get_weights(INeurodes, Coord, CPPPids, CEPPids, [UW | Acc]);
get_weights([], _Coord, _CPPPids, _CEPPids, Acc) ->
  lists:reverse(Acc).

static_fanout([CPPPid | CPPPids], ICoord, Coord) ->
  substrate_cpp:neurode_coordinates(CPPPid, self(), ICoord, Coord),
  static_fanout(CPPPids, ICoord, Coord);
static_fanout([], _ICoord, _Coord) ->
  done.

fanin([CEPPid | CEPPids], W) ->
  receive
    {handle, {CEPPid, Command, Signal}} ->
      UW = handle(Command, {Signal, W}),
      fanin(CEPPids, UW)
    after 3000 ->
      logr:warning({substrate, fanin, error, "message not received", []})
  end;
fanin([], W) ->
  W.

get_weights([{ICoord, I, _IWeights} | INeurodes], Coord, CPPPids, CEPPids, Acc, [W | Weights], O) ->
  plasticity_fanout(CPPPids, ICoord, Coord, [I, O, W]),
  UW = fanin(CEPPids, W),
  get_weights(INeurodes, Coord, CPPPids, CEPPids, [UW | Acc], Weights, O);
get_weights([], _Coord, _CPPPids, _CEPPids, Acc, [], _O) ->
  lists:reverse(Acc).

plasticity_fanout([CPPPid | CPPPids], ICoord, Coord, IOW) ->
  substrate_cpp:neurode_coordinates_iow(CPPPid, self(), ICoord, Coord, IOW),
  plasticity_fanout(CPPPids, ICoord, Coord, IOW);
plasticity_fanout([], _ICoord, _Coord, _IOW) ->
  done.

calculate_substrate_output(IHyperlayer, PHyperlayer, LinkForm, Plasticity, CPPPids, CEPPids) ->
  case LinkForm of
    l2l_feedforward ->
      calculate_output_std(IHyperlayer, PHyperlayer, Plasticity, CPPPids, CEPPids, []);
    fully_interconnected ->
      calculate_output_fi(lists:flatten([IHyperlayer | PHyperlayer]),
        PHyperlayer, Plasticity, CPPPids, CEPPids, []);
    jordan_recurrent ->
      [OHyperlayer | _] = lists:reverse(PHyperlayer, Plasticity),
      calculate_output_std(lists:flatten([IHyperlayer | OHyperlayer]),
        PHyperlayer, Plasticity, CPPPids, CEPPids, []);
    neuronself_recurrent ->
      calculate_output_nsr(IHyperlayer, PHyperlayer, Plasticity, CPPPids, CEPPids, [])
  end.

calculate_output_std(INeurodes, [CurHyperlayer | Substrate], Plasticity, CPPPids, CEPPids, Acc) ->
  UCurHyperlayer = [calculate_output(INeurodes, Neurode, Plasticity, CPPPids, CEPPids)
  || Neurode <- CurHyperlayer],
  calculate_output_std(UCurHyperlayer, Substrate,
    Plasticity, CPPPids, CEPPids, [UCurHyperlayer | Acc]);
calculate_output_std(OutputHyperlayer, [], _Plasticity, _CPPPids, _CEPPids, Acc) ->
  {[Output || {_Coord, Output, _Weights} <- OutputHyperlayer], lists:reverse(Acc)}.

calculate_output(INeurodes, Neurode, Plasticity, CPPPids, CEPPids) ->
  {Coord, _PrevO, Weights} = Neurode,
  case Plasticity of
    none ->
      Output = calculate_neurode_output_std(INeurodes, Neurode, 0),
      {Coord, Output, Weights};
    iterative ->
      Output = calculate_neurode_output_std(INeurodes, Neurode, 0),
      UWeights = get_weights(INeurodes, Coord, CPPPids, CEPPids, [], Weights, Output),
      {Coord, Output, UWeights};
    abcn ->
      Output = calculate_neurode_output_plast(INeurodes, Neurode, 0),
      update_neurode(INeurodes, {Coord, Output, Weights}, [])
  end.

calculate_neurode_output_std([{_ICoord, O, _IWeights} | INeurodes],
  {Coord, PrevO, [Weight | Weights]}, Acc) ->
  calculate_neurode_output_std(INeurodes, {Coord, PrevO, Weights}, O * Weight + Acc);
calculate_neurode_output_std([], {_Coord, _PrevO, []}, Acc) ->
  functions:tanh(Acc).

calculate_neurode_output_plast([{_ICoord, O, _IWeights} | INeurodes],
  {Coord, PrevO, [{W, _LF, _Parameters} | WPs]}, Acc) ->
  calculate_neurode_output_plast(INeurodes, {Coord, PrevO, WPs}, O * W + Acc);
calculate_neurode_output_plast([], {_Coord, _PrevO, []}, Acc) ->
  functions:tanh(Acc).

update_neurode([{_ICoord, IO, _IWeights} | INeurodes],
  {Coord, O, [{W, LF, Parameters} | WPs]}, Acc) ->
  UW = substrate:LF(IO, O, W, Parameters),
  update_neurode(INeurodes, {Coord, O, WPs}, [{UW, LF, Parameters} | Acc]);
update_neurode([], {Coord, O, []}, Acc) ->
  {Coord, O, lists:reverse(Acc)}.

abcn(Input, Output, W, [A, B, C, N]) ->
  DeltaWeight = N * (A * Input * Output + B * Input + C * Output),
  W + DeltaWeight.

calculate_output_fi(INeurodes, [CurHyperlayer | Substrate], Plasticity, CPPPids, CEPPids, Acc) ->
  UCurHyperlayer = [calculate_output(INeurodes, Neurode, Plasticity, CPPPids, CEPPids)
  || Neurode <- CurHyperlayer],
  calculate_output_fi([INeurodes | UCurHyperlayer], Substrate, Plasticity,
    CPPPids, CEPPids, [UCurHyperlayer | Acc]);
calculate_output_fi(OutputHyperlayer, [], _Plasticity, _CPPPids, _CEPPids, Acc) ->
  {[Output || {_Coord, Output, _Weights} <- OutputHyperlayer], lists:reverse(Acc)}.

calculate_output_nsr(INeurodes, [CurHyperlayer | Substrate], Plasticity, CPPPids, CEPPids, Acc) ->
  UCurHyperlayer = [calculate_output([Neurode | INeurodes], Neurode, Plasticity, CPPPids, CEPPids)
  || Neurode <- CurHyperlayer],
  calculate_output_nsr(UCurHyperlayer, Substrate, Plasticity, CPPPids, CEPPids,
    [UCurHyperlayer | Acc]);
calculate_output_nsr(OutputHyperlayer, [], _Plasticity, _CPPPids, _CEPPids, Acc) ->
  {[Output || {_Coord, Output, _Weights} <- OutputHyperlayer], lists:reverse(Acc)}.