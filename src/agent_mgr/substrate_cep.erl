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
-module(substrate_cep).

%% Start/Stop
-export([
  start/2,
  stop/2
]).

%% API
-export([
  init_phase2/8,
  forward/3
]).

%% Callbacks
-export([
  init/1,
  loop/1, loop/8,
  terminate/1
]).

%% Xref
-ignore_xref([
  loop/1, loop/8,
  handle/2,
  forward/3,
  terminate/1
]).

%%%============================================================================
%%% API
%%%============================================================================

%%-----------------------------------------------------------------------------
%% @doc Spawns a Substrate_CEP process belonging to the exoself process that
%%      spawned it and calls init to initialize.
%% @end
%%-----------------------------------------------------------------------------
-spec start(node(), pid()) -> pid().
start(Node, ExoselfPid) ->
  spawn_link(Node, ?MODULE, init, [ExoselfPid]).

%%-----------------------------------------------------------------------------
%% @doc Terminates the Substrate_CEP.
%% @end
%%-----------------------------------------------------------------------------
-spec stop(pid(), pid()) -> ok.
stop(Pid, ExoselfPid) ->
  Pid ! {ExoselfPid, stop},
  ok.

%%-----------------------------------------------------------------------------
%% @doc Initializes the substrate_cep.
%% @end
%%-----------------------------------------------------------------------------
-spec init_phase2(pid(), pid(), models:substrate_id(), pid(), pid(), atom(), [float()], [pid()])
-> ok.
init_phase2(Pid, ExoselfPid, Id, CxPid, SubstratePid, CEPName, Parameters, FaninPids) ->
  Pid ! {handle, {init_phase2, ExoselfPid, Id, CxPid, SubstratePid,
  CEPName, Parameters, FaninPids}},
  ok.

%%-----------------------------------------------------------------------------
%% @doc The neurons in the output layer of the NN produce output signals,
%%      which are then sent to the CEPs they are connected to.
%%      The CEPs wait and gather the signals from all the neurons with whom
%%      they have presynaptic links. The CEPs process the accumulated signals.
%%      The CEPs forward the vector signals to the substrate.
%% @end
%%-----------------------------------------------------------------------------
-spec forward(pid(), pid(), [float()]) -> ok.
forward(Pid, IPid, Input) ->
  Pid ! {handle, {forward, IPid, Input}},
  ok.

%%%============================================================================
%%% Callbacks
%%%============================================================================

%%-----------------------------------------------------------------------------
%% @private
%% @doc Whenever a Substrate_CEP process is started via the start function this
%%      function is called by the new process to initialize.
%% @end
%%-----------------------------------------------------------------------------
-spec init(pid()) -> no_return().
init(ExoselfPid) ->
  utils:random_seed(),
  logr:debug({substrate_cep, init, ok, undefined, []}),
  loop(ExoselfPid).

%%-----------------------------------------------------------------------------
%% @private
%% @doc Receive and handle messages.
%% @end
%%-----------------------------------------------------------------------------
-spec loop(pid()) -> no_return().
loop(ExoselfPid) ->
  receive
    {handle, {init_phase2, ExoselfPid, Id, CxPid, SubstratePid, CEPName, Parameters, FaninPids}} ->
      loop(Id, ExoselfPid, CxPid, SubstratePid, CEPName, Parameters, {FaninPids, FaninPids}, [])
  end.

%%-----------------------------------------------------------------------------
%% @doc The substrate_cep process gathers the control signals from the
%%      neurons, appending them to the accumulator. The order in which the
%%      signals are accumulated into a vector is in the same order that the
%%      neuron ids are stored within NIds. Once all the signals have been
%%      gathered, the substrate_cep executes its function, forwards the
%%      processed signal to the substrate, and then again begins to wait
%%      for the neural signals from the output layer by reseting the
%%      FaninPids from the second copy of the list.
%% @end
%%-----------------------------------------------------------------------------
-spec loop(models:substrate_id(), pid(), pid(), pid(), atom(), [float()],
  {[pid()], [pid()]}, [pid()]) -> no_return().
loop(Id, ExoselfPid, CxPid, SubstratePid, CEPName,
  Parameters, {[FromPid | FaninPids], MFaninPids}, Acc) ->
  receive
    {handle, {forward, FromPid, Input}} ->
      logr:debug({substrate_cep, msg, ok, "SIPid forward message received", [FromPid]}),
      loop(Id, ExoselfPid, CxPid, SubstratePid, CEPName, Parameters, {FaninPids, MFaninPids},
        lists:append(Input, Acc));
    {forward, FromPid, Input} ->
      logr:debug({substrate_cep, msg, ok, "SIPid forward message received", [FromPid]}),
      loop(Id, ExoselfPid, CxPid, SubstratePid, CEPName, Parameters, {FaninPids, MFaninPids},
        lists:append(Input, Acc));
    {ExoselfPid, stop} ->
      terminate(normal)
  end;

loop(Id, ExoselfPid, CxPid, SubstratePid, CEPName, Parameters, {[], MFaninPids}, Acc) ->
  ProperlyOrderedInput = lists:reverse(Acc),
  case CEPName of
    set_weight ->
      set_weight(ProperlyOrderedInput, Parameters, SubstratePid);
    set_abcn ->
      set_abcn(ProperlyOrderedInput, Parameters, SubstratePid);
    delta_weight ->
      delta_weight(ProperlyOrderedInput, Parameters, SubstratePid)
  end,
  loop(Id, ExoselfPid, CxPid, SubstratePid, CEPName, Parameters, {MFaninPids, MFaninPids}, []).

%%-----------------------------------------------------------------------------
%% @private
%% @doc This function is called to terminate the process. It performs
%%      any necessary cleaning up before exiting with the << Reason >>
%%      parameter that it was called with.
%% @end
%%-----------------------------------------------------------------------------
-spec terminate(atom()) -> ok.
terminate(Reason) ->
  logr:debug({substrate_cep, terminate, ok, undefined, [Reason]}),
  exit(Reason).

%%%============================================================================
%%% Internal functions
%%%============================================================================

set_weight(Output, _Parameters, SubstratePid) ->
  [Val] = Output,
  Threshold = 0.33,
  Weight = calc_weight(Val, Threshold),
  substrate:set_weight(SubstratePid, self(), [Weight]).

set_abcn(Output, _Parameters, SubstratePid) ->
  substrate:set_abcn(SubstratePid, self(), Output).

delta_weight(Output, _Parameters, SubstratePid) ->
  [Val] = Output,
  Threshold = 0.33,
  DW = calc_weight(Val, Threshold),
  substrate:set_iterative(SubstratePid, self(), [DW]).

calc_weight(Val, Threshold) ->
  case Val > Threshold of
    true ->
      (functions:scale(Val, 1.0, Threshold) + 1.0) / 2.0;
    false ->
      case Val < -Threshold of
        true ->
          (functions:scale(Val, -Threshold, -1.0) -1.0) / 2.0;
        false ->
          0.0
      end
  end.