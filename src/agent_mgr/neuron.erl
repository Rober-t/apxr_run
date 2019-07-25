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
%%% @doc The neuron is a signal processing element. It accepts signals,
%%%      accumulates them into an ordered vector, then processes this input
%%%      vector to produce an output, and finally passes the output to other
%%%      elements it is connected to. The neuron never interacts with the
%%%      environment directly, and even when it does receive signals and
%%%      produces output signals, it does not know whether these input signals
%%%      are coming from sensors or neurons, or whether it is sending its
%%%      output signals to other neurons or actuators. All the neuron does is
%%%      have a list of of input Pids from which it expects to receive
%%%      signals, a list of output Pids to which the neuron sends its output,
%%%      a weight list correlated with the input Pids, and an activation
%%%      function it applies to the dot product of the input vector and its
%%%      weight vector. The neuron waits until it receives all the input
%%%      signals, and then passes the output onwards.
%%%      NOTE: The neuron is the basic processing element, the basic processing
%%%      node in the neural network system. The neurons in this system we’ve
%%%      created are more general than those used by others. They can easily
%%%      use various activation functions, and to accept and output vectors.
%%%      Because we can use anything for the activation function, including
%%%      logical operators, the neurons are really just processing nodes. In
%%%      some sense, this system is not a Topology and Weight Evolving
%%%      Artificial Neural Network, but a Topology and Parameter Evolving
%%%      Universal Learning Network (TPEULN). Nevertheless, we will continue
%%%      referring to these processing elements as neurons.
%%% @end
%%%----------------------------------------------------------------------------
-module(neuron).

%% Start/Stop
-export([
  start/2,
  stop/2
]).

%% API
-export([
  init_phase2/12,
  forward/3,
  weight_backup/2,
  weight_restore/2,
  weight_perturb/3,
  reset_prep/2,
  get_backup/2,
  perturb_pf/2,
  perturb_weights_p/4
]).

%% Callbacks
-export([
  init/1,
  loop/1, loop/6,
  handle/2,
  terminate/1
]).

%% Xref
-ignore_xref([
  loop/1, loop/6,
  handle/2,
  perturb_pf/2,
  perturb_weights_p/4,
  terminate/1
]).

%%%============================================================================
%%% Types
%%%============================================================================

-record(neuron_state, {
  id :: models:neuron_id(),
  cx_pid :: pid(),
  af :: models:neural_af(),
  aggr_f :: atom(),
  heredity_type :: darwinian | lamarckian,
  si_pids :: [pid()] | [ok],
  % The input_pids that are currently effective and represent the neuron's
  % processing dynamics.
  si_pidps_current :: [{pid(), [float()]}],
  % A second input_pids list, which represents the state of input_pids right
  % after perturbation, before the synaptic weights are affected by the
  % neuron's plasticity function (bl -> before learning). When a neuron is
  % requested to to perturb its synaptic weights, right after the weights are
  % perturbed, we want to save this new input_pids list, before plasticity gets
  % a chance to modify the synaptic weights. Afterwards, the neuron can process
  % the input signals using its input_pids_current, and its learning rule can
  % affect the input_pids_current list. But input_pids_bl will remain unchanged.
  si_pidps_bl :: [{pid(), [float()]}],
  % When a neuron is sent the weight_backup message, it is here that
  % heredity_type plays its role. When its << darwinian >>, the neuron saves the
  % input_pids_bl to input_pids_backup, instead of the input_pids_current which
  % could have been modified by some learning rule by this point. On the other
  % hand, when the heredity_type is << lamarckian >>, the neuron saves the
  % input_pids_current to input_pids_backup. The input_pids_current represents
  % the synaptic weights that could have been updated if the neuron allows for
  % plasticity, and thus the input_pids_backup will then contain not the initial
  % state of the synaptic weight list with which the neuron started, but the
  % state of the synaptic weights after the the neuron has experienced,
  % processed, and had its synaptic weights modified by its learning rule.
  si_pidps_backup :: [{pid(), [float()]}],
  mi_pids :: [pid()] | [ok],
  mi_pidps_current :: [{pid(), [float()]}],
  mi_pidps_backup :: [{pid(), [float()]}],
  pf_current :: {models:neural_pfn(), [float()]},
  pf_backup :: {models:neural_pfn(), [float()]},
  output_pids :: [pid()],
  ro_pids :: [pid()]
}).

-type state() :: #neuron_state{}.

%%%============================================================================
%%% Configuration
%%%============================================================================

-define(SAT_LIMIT, math:pi() * 2).

%%%============================================================================
%%% API
%%%============================================================================

%%-----------------------------------------------------------------------------
%% @doc Spawns a Neuron process belonging to the Exoself process that
%%      spawned it and calls init to initialize.
%% @end
%%-----------------------------------------------------------------------------
-spec start(node(), pid()) -> pid().
start(Node, ExoselfPid) ->
  spawn_link(Node, ?MODULE, init, [ExoselfPid]).

%%-----------------------------------------------------------------------------
%% @doc Terminates neuron.
%% @end
%%-----------------------------------------------------------------------------
-spec stop(pid(), pid()) -> ok.
stop(Pid, ExoselfPid) ->
  Pid ! {ExoselfPid, stop},
  ok.

%%-----------------------------------------------------------------------------
%% @doc Initializes the neuron setting it to its initial state.
%% @end
%%-----------------------------------------------------------------------------
-spec init_phase2(pid(), pid(), models:neuron_id(), pid(), models:neural_af(),
  {models:neural_pfn(), [float()]}, atom(), darwinian | lamarckian, [tuple()], [tuple()], [pid()],
  [pid()]) -> ok.
init_phase2(Pid, ExoselfPid, Id, CxPid, AF, PF, AggrF, HeredityType, SIPidPs, MIPidPs, OutputPids,
  ROPids) ->
    Pid ! {handle, {init_phase2, ExoselfPid, Id, CxPid, AF, PF, AggrF, HeredityType, SIPidPs,
    MIPidPs, OutputPids, ROPids}},
    ok.

%%-----------------------------------------------------------------------------
%% @doc The Neuron process waits for vector signals from all the processes
%%      that it's connected from, taking the dot product of the input and
%%      weight vectors, and then adding it to the accumulator. Once all the
%%      signals from InputPids are received, the accumulator contains the
%%      dot product to which the neuron then adds the bias and executes the
%%      activation function. After fanning out the output signal, the neuron
%%      again returns to waiting for incoming signals.
%% @end
%%-----------------------------------------------------------------------------
-spec forward(pid(), pid(), float()) -> ok.
forward(Pid, IPid, Input) ->
  Pid ! {handle, {forward, IPid, Input}},
  ok.

%%-----------------------------------------------------------------------------
%% @doc weight_backup The signal from the exoself, which tells the neuron
%%      that the NN system performs best when this particular neuron is using
%%      its current synaptic weight combination, and thus it should save this
%%      synaptic weight list as MInputPidPs, and that it is the best weight
%%      combination achieved thus far. The message is sent if after the weight
%%      perturbation, the NN's evaluation achieves a higher fitness than when
%%      the neurons of this NN used their previous synaptic weights.
%% @end
%%-----------------------------------------------------------------------------
-spec weight_backup(pid(), pid()) -> ok.
weight_backup(Pid, ExoselfPid) ->
  Pid ! {handle, {ExoselfPid, weight_backup}},
  ok.

%%-----------------------------------------------------------------------------
%% @doc weight_restore This message is sent from the exoself, and it tells
%%      the neuron that it should restore its synaptic weight list to the one
%%      previously used, saved as MInputPidPs. This message is usually sent if
%%      after the weight perturbation, the NN based agent's evaluation performs
%%      worse than it did with its previous synaptic weight combinations.
%% @end
%%-----------------------------------------------------------------------------
-spec weight_restore(pid(), pid()) -> ok.
weight_restore(Pid, ExoselfPid) ->
  Pid ! {handle, {ExoselfPid, weight_restore}},
  ok.

%%-----------------------------------------------------------------------------
%% @doc weight_perturb Uses the Spread value for the purpose of generating
%%      synaptic weight perturbations.
%% @end
%%-----------------------------------------------------------------------------
-spec weight_perturb(pid(), pid(), integer()) -> ok.
weight_perturb(Pid, ExoselfPid, Spread) ->
  Pid ! {handle, {ExoselfPid, weight_perturb, Spread}},
  ok.

%%-----------------------------------------------------------------------------
%% @doc reset_prep This is message is sent after a single evaluation is
%%      completed, and the exoself wishes to reset all the neurons to their
%%      original states, with empty inboxes. Once a neuron receives this
%%      message, it goes into a reset_prep state, flushes its buffer/inbox, and
%%      then awaits for the {ExoselfPid, reset} signal. When the neuron receives
%%      the {ExoselfPid, reset} message, it again sends out the default output
%%      message to all its recurrent connections (Ids stored in the ro_ids
%%      list), and then finally drops back into its main receive loop.
%% @end
%%-----------------------------------------------------------------------------
-spec reset_prep(pid(), pid()) -> ok.
reset_prep(Pid, ExoselfPid) ->
  Pid ! {handle, {ExoselfPid, reset_prep}},
  ok.

%%-----------------------------------------------------------------------------
%% @doc get_backup neuron sends back to the exoself its last best synaptic
%%      weight combination, stored as the MInputPids list.
%% @end
%%-----------------------------------------------------------------------------
-spec get_backup(pid(), pid()) -> ok.
get_backup(Pid, ExoselfPid) ->
  Pid ! {handle, {ExoselfPid, get_backup}},
  ok.

%%-----------------------------------------------------------------------------
%% @doc perturb_pf perturbs the plasticity function.
%% @end
%%----------------------------------------------------------------------------
-spec perturb_pf(float(), {atom(), [float()]}) -> {atom(), [float()]}.
perturb_pf(Spread, {PFName, PFParameters}) ->
  do_perturb_pf(Spread, {PFName, PFParameters}).

%%-----------------------------------------------------------------------------
%% @doc The perturb_weights_p function is the function that actually goes
%%      through each weight block, and perturbs each weight with a
%%      probability of MP. If the weight is chosen to be perturbed, the
%%      perturbation intensity is chosen uniformly between -Spread and
%%      Spread.
%% @end
%%-----------------------------------------------------------------------------
-spec perturb_weights_p(float(), float(), [{float(), [float()]}], [float()]) -> [float()].
perturb_weights_p(Spread, MP, [{W, LPs} | Weights], Acc) ->
  do_perturb_weights_p(Spread, MP, [{W, LPs} | Weights], Acc).

%%%============================================================================
%%% Callbacks
%%%============================================================================

%%-----------------------------------------------------------------------------
%% @private
%% @doc Whenever a Neuron process is started via the start function this
%%      function is called by the new process to initialize.
%% @end
%%-----------------------------------------------------------------------------
-spec init(pid()) -> no_return().
init(ExoselfPid) ->
  utils:random_seed(),
  logr:debug({neuron, init, ok, undefined, []}),
  loop(ExoselfPid).

%%-----------------------------------------------------------------------------
%% @private
%% @doc During initialization the neuron sends out the default forward signals
%%      to any elements in its ro_ids list, if any.
%% @end
%%-----------------------------------------------------------------------------
-spec loop(pid()) -> no_return().
loop(ExoselfPid) ->
  receive
    {handle, {init_phase2, ExoselfPid, Id, CxPid, AF, PF, AggrF, HeredityType, SIPidPs, MIPidPs,
    OutputPids, ROPids}} ->
      SIPids = append_ipids(SIPidPs),
      MIPids = append_ipids(MIPidPs),
      NewState = handle(init_phase2, {Id, CxPid, AF, PF, AggrF, HeredityType, SIPidPs, MIPidPs,
        OutputPids, ROPids, SIPids, MIPids}),
      loop(NewState, ExoselfPid, SIPids, MIPids, [], [])
  end.

%%-----------------------------------------------------------------------------
%% @private
%% @doc Receive and handle messages.
%% @end
%%-----------------------------------------------------------------------------
-spec loop(state(), pid(), [ok] | [pid()], [ok] | [pid()], [{pid(), [float()]}] | [],
  [{pid(), [float()]}] | []) -> no_return().
loop(State, ExoselfPid, [ok], [ok], SIAcc, MIAcc) ->
  NewState = handle(forward_output, {SIAcc, MIAcc, State}),
  SIPids = NewState#neuron_state.si_pids,
  MIPids = NewState#neuron_state.mi_pids,
  loop(NewState, ExoselfPid, SIPids, MIPids, [], []);

loop(State, ExoselfPid, [SIPid | SIPids], [MIPid | MIPids], SIAcc, MIAcc) ->
  receive
    {handle, {forward, SIPid, Input}} ->
      logr:debug({neuron, msg, ok, "SIPid forward message received", [SIPid]}),
      loop(State, ExoselfPid, SIPids, [MIPid | MIPids], [{SIPid, Input} | SIAcc], MIAcc);
    {handle, {forward, MIPid, Input}} ->
      logr:debug({neuron, msg, ok, "MIPid forward message received", [MIPid]}),
      loop(State, ExoselfPid, [SIPid | SIPids], MIPids, SIAcc, [{MIPid, Input} | MIAcc]);
    {forward, SIPid, Input} ->
      logr:debug({neuron, msg, ok, "SIPid forward message received", [SIPid]}),
      loop(State, ExoselfPid, SIPids, [MIPid | MIPids], [{SIPid, Input} | SIAcc], MIAcc);
    {forward, MIPid, Input} ->
      logr:debug({neuron, msg, ok, "MIPid forward message received", [MIPid]}),
      loop(State, ExoselfPid, [SIPid | SIPids], MIPids, SIAcc, [{MIPid, Input} | MIAcc]);
    {handle, {ExoselfPid, weight_backup}} ->
      NewState = handle(weight_backup, State),
      loop(NewState, ExoselfPid, [SIPid | SIPids], [MIPid | MIPids], SIAcc, MIAcc);
    {handle, {ExoselfPid, weight_restore}} ->
      NewState = handle(weight_restore, State),
      loop(NewState, ExoselfPid, [SIPid | SIPids], [MIPid | MIPids], SIAcc, MIAcc);
    {handle, {ExoselfPid, weight_perturb, Spread}} ->
      NewState = handle(weight_perturb, {State, Spread}),
      loop(NewState, ExoselfPid, [SIPid | SIPids], [MIPid | MIPids], SIAcc, MIAcc);
    {handle, {ExoselfPid, reset_prep}} ->
      flush_buffer(),
      ExoselfPid ! {self(), ready},
      ROPids = State#neuron_state.ro_pids,
      receive
        {ExoselfPid, reset} ->
          logr:debug({neuron, reset, ok, "Fanning out ROPids", [ROPids]}),
          fanout(ROPids);
        {ExoselfPid, stop} ->
          terminate(normal)
      end,
      loop(State, ExoselfPid, State#neuron_state.si_pids, State#neuron_state.mi_pids, [], []);
    {handle, {ExoselfPid, get_backup}} ->
      handle(get_backup, {State, ExoselfPid}),
      loop(State, ExoselfPid, [SIPid | SIPids], [MIPid | MIPids], SIAcc, MIAcc);
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
  logr:debug({neuron, terminate, ok, undefined, [Reason]}),
  exit(Reason).

%%%============================================================================
%%% Internal functions
%%%============================================================================

handle(init_phase2, {Id, CxPid, AF, PF, AggrF, HeredityType, SIPidPs, MIPidPs, OutputPids, ROPids,
  SIPids, MIPids}) ->
    fanout(ROPids),
    logr:debug({neuron, init2, ok, undefined, []}),
    State = #neuron_state{
      id = Id,
      cx_pid = CxPid,
      af = AF,
      pf_current = PF,
      pf_backup = PF,
      aggr_f = AggrF,
      heredity_type = HeredityType,
      si_pids = SIPids,
      si_pidps_bl = SIPidPs,
      si_pidps_current = SIPidPs,
      si_pidps_backup = SIPidPs,
      mi_pids = MIPids,
      mi_pidps_current = MIPidPs,
      mi_pidps_backup = MIPidPs,
      output_pids = OutputPids,
      ro_pids = ROPids
    },
    State;
handle(forward_output, {SIAcc, MIAcc, State}) ->
  OutputSatLimit = app_config:get_env(output_sat_limit),
  {PFName, PFParameters} = State#neuron_state.pf_current,
  AF = State#neuron_state.af,
  AggrF = State#neuron_state.aggr_f,
  OrderedSIAcc = lists:reverse(SIAcc),
  SIPidPs = State#neuron_state.si_pidps_current,
  SOutput = [sat(functions:AF(signal_aggregator:AggrF(OrderedSIAcc, SIPidPs)), OutputSatLimit)],
  NewState = case PFName of
    none ->
      State;
    _ ->
      OrderedMIAcc = lists:reverse(MIAcc),
      MIPidPs = State#neuron_state.mi_pidps_current,
      MAggregationProduct = signal_aggregator:dot_product(OrderedMIAcc, MIPidPs),
      MOutput = sat(functions:tanh(MAggregationProduct), ?SAT_LIMIT),
      USIPidPs = plasticity:PFName([MOutput | PFParameters], OrderedSIAcc, SIPidPs, SOutput),
      State#neuron_state{si_pidps_current = USIPidPs}
  end,
  OutputPids = State#neuron_state.output_pids,
  [OutputPid ! {forward, self(), SOutput} || OutputPid <- OutputPids], % Actuator or Neuron.
  logr:debug({neuron, forward_output, ok, undefined, []}),
  NewState;
handle(weight_backup, State) ->
  NewState = case State#neuron_state.heredity_type of
    darwinian ->
      State#neuron_state{
        si_pidps_backup = State#neuron_state.si_pidps_bl,
        mi_pidps_backup = State#neuron_state.mi_pidps_current,
        pf_backup = State#neuron_state.pf_current
      };
    lamarckian ->
      State#neuron_state{
        si_pidps_backup = State#neuron_state.si_pidps_current,
        mi_pidps_backup = State#neuron_state.mi_pidps_current,
        pf_backup = State#neuron_state.pf_current
      }
  end,
  logr:debug({neuron, weight_backup, ok, undefined, []}),
  NewState;
handle(weight_restore, State) ->
  NewState = State#neuron_state{
    si_pidps_bl = State#neuron_state.si_pidps_backup,
    si_pidps_current = State#neuron_state.si_pidps_backup,
    mi_pidps_current = State#neuron_state.mi_pidps_backup,
    pf_current = State#neuron_state.pf_backup
  },
  logr:debug({neuron, weight_restore, ok, undefined, []}),
  NewState;
handle(weight_perturb, {State, Spread}) ->
  PerturbedSIPidPs = perturb_ipidps(Spread, State#neuron_state.si_pidps_backup),
  PerturbedMIPidPs = perturb_ipidps(Spread, State#neuron_state.mi_pidps_backup),
  PerturbedPF = perturb_pf(Spread, State#neuron_state.pf_backup),
  NewState = State#neuron_state{
    si_pidps_bl = PerturbedSIPidPs,
    si_pidps_current = PerturbedSIPidPs,
    mi_pidps_current = PerturbedMIPidPs,
    pf_current = PerturbedPF
  },
  logr:debug({neuron, weight_perturb, ok, undefined, []}),
  NewState;
handle(get_backup, {State, ExoselfPid}) ->
  NId = State#neuron_state.id,
  ExoselfPid ! {self(), NId, State#neuron_state.si_pidps_backup,
  State#neuron_state.mi_pidps_backup, State#neuron_state.pf_backup},
  logr:debug({neuron, get_backup, ok, undefined, []}).

do_perturb_pf(Spread, {PFName, PFParameters}) ->
  UPFParameters = [sat(PFParameter + (rand:uniform() - 0.5) * Spread, -?SAT_LIMIT, ?SAT_LIMIT) ||
  PFParameter <- PFParameters],
  {PFName, UPFParameters}.

append_ipids(IPidPs) ->
  lists:append([IPid || {IPid, _W} <- IPidPs, IPid =/= bias], [ok]).

%%----------------------------------------------------------------------------
%% @private
%% @doc The perturb_ipidps function calculates the probability with which
%%      each neuron in the InputPidPs is chosen to be perturbed. The
%%      probability is based on the total number of weights in the
%%      InputPidPs list, with the actual mutation probability equating to
%%      the inverse of square root of total number of weights.
%% @end
%%----------------------------------------------------------------------------
perturb_ipidps(_Spread, []) ->
  [];
perturb_ipidps(Spread, InputPidPs) ->
  TotWeights = lists:sum([length(WeightsP) || {_InputPid, WeightsP} <- InputPidPs]),
  MP = 1 / math:sqrt(TotWeights),
  perturb_ipidps(Spread, MP, InputPidPs, []).

%%----------------------------------------------------------------------------
%% @private
%% @doc The perturb_ipidps function calculates the probability with which
%%      each neuron in the InputPidPs is chosen to be perturbed. The
%%      probability is based on the total number of weights in the
%%      InputPidPs list, with the actual mutation probability equating to
%%      the inverse of square root of total number of weights.
%% @end
%%----------------------------------------------------------------------------
perturb_ipidps(Spread, MP, [{InputPid, WeightsP} | InputPidPs], Acc) ->
  UWeightsP = do_perturb_weights_p(Spread, MP, WeightsP, []),
  perturb_ipidps(Spread, MP, InputPidPs, [{InputPid, UWeightsP} | Acc]);
perturb_ipidps(_Spread, _MP, [], Acc) ->
  lists:reverse(Acc).

%%----------------------------------------------------------------------------
%% @private
%% @doc The do_perturb_weights_p function goes through each weights block and
%%      calls the do_perturb_weights_p to perturb the weights.
%% @end
%%----------------------------------------------------------------------------
do_perturb_weights_p(Spread, MP, [{W, LPs} | Weights], Acc) ->
  UW = case rand:uniform() < MP of
    true ->
      sat((rand:uniform() -0.5) * 2 * Spread + W, -?SAT_LIMIT, ?SAT_LIMIT);
    false ->
      W
  end,
  do_perturb_weights_p(Spread, MP, Weights, [{UW, LPs} | Acc]);
do_perturb_weights_p(_Spread, _MP, [], Acc) ->
  lists:reverse(Acc).

%%-----------------------------------------------------------------------------
%% @private
%% @doc The fanout function fans out the Msg to all the Pids in its list.
%% @end
%%-----------------------------------------------------------------------------
fanout([Pid | Pids]) ->
  ROSignal = app_config:get_env(ro_signal),
  Pid ! {forward, self(), ROSignal},
  fanout(Pids);
fanout([])->
  true.

%%-----------------------------------------------------------------------------
%% @private
%% @doc The flush_buffer cleans out the element's inbox.
%% @end
%%-----------------------------------------------------------------------------
flush_buffer() ->
  receive
    _ ->
      flush_buffer()
  after 0 ->
    done
end.

%%-----------------------------------------------------------------------------
%% @private
%% @doc The sat function simply ensures that the Val is neither less than
%%      min or greater than max.
%% @end
%%-----------------------------------------------------------------------------
sat(Val, Limit) ->
  sat(Val, -Limit, Limit).
sat(Val, Min, _Max) when Val < Min ->
  Min;
sat(Val, _Min, Max) when Val > Max ->
  Max;
sat(Val, _Min, _Max) ->
  Val.