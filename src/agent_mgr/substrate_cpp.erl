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
%%% @doc The substrate polls the substrate_cpps (Substrate Coordinate
%%%      PreProcessor), and then waits for the signals from the
%%%      substrate_ceps (Substrate Connectivity Expression Producer)
%%%      process, which tells it what the synaptic weight is between
%%%      the two neurodes with which the substrate_cpps were called
%%%      with, and whether the connection between these neurodes is
%%%      expressed or not.
%%%      The substrate_cpp and substrate_cep processes are
%%%      analogous to the sensors and actuators respectively, but
%%%      driven and polled by the substrate when it wishes to
%%%      calculate the synaptic weights and connectivity expression
%%%      between its various neurodes.
%%%      The substrate will forward to its one or more substrate_cpps
%%%      the coordinates of the two connected neurodes in question,
%%%      the called substrate_cpp will process those coordinates based
%%%      on its type and forward the processed vector to the NN. The
%%%      substrate will then wait for the signals from its one or more
%%%      substrate_ceps, which will provide it with the various signals
%%%      which the substrate will then use to set its synaptic weights,
%%%      connectivity expressions, or even plasticity based synaptic
%%%      weight updates.
%%%      The substrate uses its substrate_cpps and substrate_ceps for every
%%%      synaptic weight/expression it wishes to set or update. Unlike the
%%%      sensors and actuators, the substrate_cpps and substrate_ceps do
%%%      not need to sync up with the cortex because the substrate_cpps are
%%%      not be triggered by the cortex, and because the signals from
%%%      substrate_ceps are awaited by the substrate, and since the substrate
%%%      itself only processes signals once it has received all the sensory
%%%      signals from the sensors which themselves are triggered by the cortex,
%%%      the whole system is synchronized.
%%% @end
%%%----------------------------------------------------------------------------
-module(substrate_cpp).

%% Start/Stop
-export([
  start/2,
  stop/2
]).

%% API
-export([
  init_phase2/9,
  neurode_coordinates/4,
  neurode_coordinates_iow/5
]).

%% Callbacks
-export([
  init/1,
  loop/1, loop/8,
  handle/2,
  terminate/1
]).

%% Xref
-ignore_xref([
  loop/1, loop/8,
  handle/2,
  terminate/1
]).

%%%============================================================================
%%% API
%%%============================================================================

%%-----------------------------------------------------------------------------
%% @doc Spawns a Substrate_CPP process belonging to the exoself process that
%%      spawned it and calls init to initialize.
%% @end
%%-----------------------------------------------------------------------------
-spec start(node(), pid()) -> pid().
start(Node, ExoselfPid) ->
  spawn_link(Node, ?MODULE, init, [ExoselfPid]).

%%-----------------------------------------------------------------------------
%% @doc Terminates substrate_cpp.
%% @end
%%-----------------------------------------------------------------------------
-spec stop(pid(), pid()) -> ok.
stop(Pid, ExoselfPid) ->
  Pid ! {ExoselfPid, stop},
  ok.

%%-----------------------------------------------------------------------------
%% @doc Initializes substrate_cpp.
%% @end
%%-----------------------------------------------------------------------------
-spec init_phase2(pid(), pid(), models:substrate_id(), pid(), pid(), atom(), integer(),
  [float()], [pid()]) -> ok.
init_phase2(Pid, ExoselfPid, Id, CxPid, SubstratePid, CPPName, VL, Parameters, FanoutPids) ->
  Pid ! {handle, {init_phase2, ExoselfPid, Id, CxPid, SubstratePid, CPPName, VL,
  Parameters, FanoutPids}},
  ok.

%%-----------------------------------------------------------------------------
%% @doc The substrate sends the coordinates of the connected neurodes to the
%%      substrate_cpps it is connected to. The CPPs process the coordinates.
%%      The CPPs forward the processed coordinate vectors to the neurons they
%%      are connected to in the NN. The NN processes the coordinate signals.
%% @end
%%-----------------------------------------------------------------------------
-spec neurode_coordinates(pid(), pid(), [float()], [float()]) -> ok.
neurode_coordinates(Pid, SubstratePid, PresynapticCoords, PostsynapticCoords) ->
  Pid ! {handle, {neurode_coordinates, SubstratePid, PresynapticCoords, PostsynapticCoords}},
  ok.

%%-----------------------------------------------------------------------------
%% @doc IOW = Input, Output and Weight.
%%      The substrate sends the coordinates of the connected neurodes to the
%%      substrate_cpps it is connected to. The CPPs process the coordinates.
%%      The CPPs forward the processed coordinate vectors to the neurons they
%%      are connected to in the NN. The NN processes the coordinate signals.
%% @end
%%-----------------------------------------------------------------------------
-spec neurode_coordinates_iow(pid(), pid(), [float()], [float()], [float()]) -> ok.
neurode_coordinates_iow(Pid, SubstratePid, PresynapticCoords, PostsynapticCoords, IOW) ->
  Pid ! {handle, {neurode_coordinates, SubstratePid, PresynapticCoords, PostsynapticCoords, IOW}},
  ok.

%%%============================================================================
%%% Callbacks
%%%============================================================================

%%-----------------------------------------------------------------------------
%% @private
%% @doc Whenever a Substrate_CPP process is started via the start function this
%%      function is called by the new process to initialize.
%% @end
%%-----------------------------------------------------------------------------
-spec init(pid()) -> no_return().
init(ExoselfPid) ->
  utils:random_seed(),
  logr:debug({substrate_cpp, init, ok, undefined, []}),
  loop(ExoselfPid).

%%-----------------------------------------------------------------------------
%% @private
%% @doc Receive and handle messages.
%% @end
%%-----------------------------------------------------------------------------
-spec loop(pid()) -> no_return().
loop(ExoselfPid) ->
  receive
    {handle, {init_phase2, ExoselfPid, Id, CxPid, SubstratePid,
    CPPName, VL, Parameters, FanoutPids}} ->
      loop(Id, ExoselfPid, CxPid, SubstratePid, CPPName, VL, Parameters, FanoutPids)
  end.

%%-----------------------------------------------------------------------------
%% @private
%% @doc Receive and handle messages.
%% @end
%%-----------------------------------------------------------------------------
-spec loop(models:substrate_id(), pid(), pid(), pid(), atom(), integer(), [float()], [pid()])
-> no_return().
loop(Id, ExoselfPid, CxPid, SubstratePid, CPPName, VL, Parameters, FanoutPids) ->
  receive
    {handle, {neurode_coordinates, SubstratePid, PresynapticCoords, PostsynapticCoords}} ->
      logr:debug({substrate_cpp, msg, ok, "neurode_coordinates",
        [PresynapticCoords, PostsynapticCoords]}),
      handle(neurode_coordinates, {CPPName, FanoutPids, PresynapticCoords, PostsynapticCoords}),
      loop(Id, ExoselfPid, CxPid, SubstratePid, CPPName, VL, Parameters, FanoutPids);
    {handle, {neurode_coordinates, SubstratePid, PresynapticCoords, PostsynapticCoords, IOW}} ->
      logr:debug({substrate_cpp, msg, ok, "neurode_coordinates_iow",
        [PresynapticCoords, PostsynapticCoords]}),
      handle(neurode_coordinates_iow, {CPPName, FanoutPids, PresynapticCoords,
        PostsynapticCoords, IOW}),
      loop(Id, ExoselfPid, CxPid, SubstratePid, CPPName, VL, Parameters, FanoutPids);
    {ExoselfPid, stop} ->
      terminate(normal)
  end.

%%-----------------------------------------------------------------------------
%% @private
%% @doc This function is called to terminate the process. It performs
%%      any necessary cleaning up before exiting with the << Reason >>
%%      parameter that it was called with.
%% @end
%%-----------------------------------------------------------------------------
-spec terminate(atom()) -> ok.
terminate(Reason) ->
  logr:debug({substrate_cpp, terminate, ok, undefined, [Reason]}),
  exit(Reason).

%%%============================================================================
%%% Internal functions
%%%============================================================================

handle(neurode_coordinates, {CPPName, FanoutPids, PresynapticCoords, PostsynapticCoords}) ->
  SensoryVector = functions:CPPName(PresynapticCoords, PostsynapticCoords),
  logr:debug({substrate_cpp, neurode_coordinates, ok, undefined, []}),
  [neuron:forward(Pid, self(), SensoryVector) || Pid <- FanoutPids];

handle(neurode_coordinates_iow, {CPPName, FanoutPids, PresynapticCoords,
  PostsynapticCoords, IOW}) ->
  SensoryVector = functions:CPPName(PresynapticCoords, PostsynapticCoords, IOW),
  logr:debug({substrate_cpp, neurode_coordinates_iow, ok, undefined, []}),
  [neuron:forward(Pid, self(), SensoryVector) || Pid <- FanoutPids].