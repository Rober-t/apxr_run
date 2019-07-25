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
%%% @doc In a way, the exoself process embodies the agent record of the
%%%      genotype. The exoself process performs assistant services that might
%%%      be needed by the NN system, it is the process that is external to the
%%%      NN, but is part of the whole NN based system, and it also has a Pid
%%%      that is unique to the agent, since an agent can have multiple sensors
%%%      and actuators, but only a single exoself. It contains information
%%%      about the entire NN based system.
%%% @end
%%%----------------------------------------------------------------------------
-module(exoself).

%% Start/Stop
-export([
  start/1
]).

%% API
-export([
  init_phase2/3,
  evaluation_complete/6
]).

%% Callbacks
-export([
  init/0,
  loop/0, loop/2,
  handle/2,
  terminate/2
]).

%% Xref
-ignore_xref([
  init/0,
  loop/0, loop/2,
  handle/2,
  terminate/2
]).

%%%============================================================================
%%% Types
%%%============================================================================

-record(exoself_state, {
  agent_id :: models:agent_id(),
  morphology :: atom() | {atom(), atom()},
  generation :: non_neg_integer() | undefined,
  ids_npids :: ets:tid(),
  cx_pid :: pid(),
  specie_id :: models:specie_id(),
  spids = [] :: [pid()],
  sids = [] :: [models:sensor_id()],
  npids = [] :: [pid()],
  nids = [] :: [models:neuron_id()],
  apids = [] :: [pid()],
  aids = [] :: [models:actuator_id()],
  private_scape_pids = [] :: [pid()],
  highest_fitness :: inf | [float()] | undefined,
  eval_acc = 0 :: integer(),
  cycle_acc = 0 :: integer(),
  time_acc = 0 :: integer(),
  max_attempts = 15 :: non_neg_integer(),
  attempt = 1 :: non_neg_integer(),
  tuning_duration_f :: {atom(), float()},
  tuning_selection_f :: models:tuning_selection_f(),
  annealing_parameter :: float(),
  perturbation_range :: float(),
  substrate_pid :: pid() | undefined,
  cpp_pids = [] :: [pid()],
  cep_pids = [] :: [pid()],
  perturbed_idps :: [{models:neuron_id(), [float()]}] | undefined,
  op_mode :: gt | validation
}).

-type state() :: #exoself_state{}.

%%%============================================================================
%%% Configuration
%%%============================================================================

-define(MIN_PIMPROVEMENT, app_config:get_env(min_pimprovement)).

%%%============================================================================
%%% API
%%%============================================================================

%%-----------------------------------------------------------------------------
%% @doc Spawns a Exoself process belonging to the AgentWorker process that
%%      spawned it and calls init to initialize.
%% @end
%%-----------------------------------------------------------------------------
-spec start(node()) -> pid().
start(Node) ->
  spawn_link(Node, ?MODULE, init, []).

%%-----------------------------------------------------------------------------
%% @doc Initializes the Exoself process.
%% @end
%%-----------------------------------------------------------------------------
-spec init_phase2(pid(), models:agent_id(), models:op_mode()) -> ok.
init_phase2(Pid, AgentId, OpMode) ->
  Pid ! {handle, {init_phase2, AgentId, OpMode}},
  ok.

%%-----------------------------------------------------------------------------
%% @doc The exoself process awaits from its cortex process the
%%      evoluation_completed message. Once the message is received, based on
%%      the fitness achieved, exoself decides whether to continue tunning the
%%      weights or terminate the system. Exoself tries to improve the fitness
%%      by perturbing/tuning the weights of its neurons, after each tuning
%%      session, the Neural Network based system performs another evaluation
%%      by interacting with the scape until completion (the NN solves a
%%      problem, or dies within the scape or...). The order of events is
%%      important: When evaluation_complete message is received, the
%%      function first checks whether the newly achieved fitness is higher
%%      than the highest fitness achieved so far. If it is not, the function
%%      sends the neurons a message to restore their weights to previous
%%      state, during which it last achieved the highest fitness instead of
%%      their current state which yielded the current lower fitness score.
%%      If on the other hand the new fitness is higher than the previously
%%      highest achieved fitness, then the function tells the neurons to
%%      backup their current weights, as these weights represent the NNs
%%      best, most fit form yet. Exoself then tells all the neurons to
%%      prepare for a reset by sending each neuron the {self(), reset_prep}
%%      message. Since the NN can have recursive connections, and the
%%      manner in which initial recursive messages are sent, it is
%%      important for each neuron to flush their buffers to be reset into
%%      an initial fresh state, which is achieved after the neurons receive
%%      the reset_prep message. The function then sends the reset message to
%%      the neurons, which returns them into their main loop. Finally, the
%%      function checks whether exoself has already tried to improve the NNs
%%      fitness a maximum S#state.max_attempts number of times. If that is the
%%      case, the exoself process backs up the updated NN (the updated, tuned
%%      weights) to database using the backup_genotype/2 function, prints to
%%      screen that it is terminating, and sends to the population_mgr the
%%      accumulated statistics (highest fitness, evaluation count,
%%      cycle count...). On the other hand, if the exoself is not yet done
%%      tuning the neural weights, it has not yet reached its ending
%%      condition, it uses a tuning_selection_function to compose a list of
%%      tuples: [{NId, Spread}...] of neuron ids and the perturbation spread
%%      values, where the spread is the range from which the perturbation is
%%      randomly chosen. The spread itself is based on the age of the selected
%%      neuron, using the annealing_factor value, which when set to 1 implies
%%      that there is no annealing, and when set to a value less than 1,
%%      decreases the Spread. Once this list of elements is composed, the
%%      exoself sends each of the neurons a message to perturb their synaptic
%%      weights using the Spread value. The exoself then reactivates the
%%      cortex, and drops back into its main loop.
%% @end
%%-----------------------------------------------------------------------------
-spec evaluation_complete(pid(), pid(), [float()], integer(), integer(), boolean()) -> ok.
evaluation_complete(Pid, CxPid, Fitness, Cycles, Time, GoalReached) ->
  Pid ! {handle, {evaluation_complete, CxPid, Fitness, Cycles, Time, GoalReached}},
  ok.

%%%============================================================================
%%% Callbacks
%%%============================================================================

%%-----------------------------------------------------------------------------
%% @private
%% @doc Whenever a Exoself process is started via the start function this
%%      function is called by the new process to initialize.
%% @end
%%-----------------------------------------------------------------------------
-spec init() -> no_return().
init() ->
  utils:random_seed(),
  process_flag(trap_exit, true),
  logr:debug({exoself, init, ok, undefined, []}),
  loop().

%%-----------------------------------------------------------------------------
%% @private
%% @doc The handle(init_phase2,...) function sets up the exoself's state
%%      before dropping into the main loop. The function first reads the agent
%%      and cortex records belonging to the AgentId NN based system. The
%%      function then reads the sensor, actuator, and neuron ids, then spawns
%%      the private scapes using the spawn_scapes function, spawns the cortex,
%%      sensor, actuator, and neuron processes, and then finally links up all
%%      these processes together using the link_... functions. Once the
%%      phenotype has been generated from the genotype, the exoself drops
%%      into its main loop.
%% @end
%%-----------------------------------------------------------------------------
-spec loop() -> no_return().
loop() ->
  receive
    {handle, {init_phase2, AgentId, OpMode}} ->
      NewState = handle(init_phase2, {AgentId, OpMode}),
      loop(NewState, OpMode)
  end.

%%-----------------------------------------------------------------------------
%% @private
%% @doc Receive and handle messages.
%% @end
%%-----------------------------------------------------------------------------
-spec loop(state(), models:op_mode()) -> no_return().
loop(State, gt) ->
  receive
    {handle, {evaluation_complete, CxPid, Fitness, Cycles, Time, GoalReached}} ->
      case handle(evaluation_complete, {State, CxPid, Fitness, Cycles, Time, GoalReached}) of
        terminate_agent ->
          terminate(terminate_agent, State);
        NewState ->
          loop(NewState, gt)
      end;
    {'EXIT', _Pid, normal} ->
      ignore;
    {'EXIT', Pid, Reason} ->
      logr:debug({exoself, msg, ok, "exit received", [Pid, Reason]}),
      terminate(peer_terminated, State)
  end;

loop(State, validation) ->
  logr:debug({exoself, validation, ok, "entered validation loop", []}),
  receive
    {handle, {evaluation_complete, _CxPid, Fitness, _Cycles, _Time, _GoalReached}} ->
      logr:info({exoself, status, ok, "validation complete. agent terminating", [Fitness]}),
      AgentId = State#exoself_state.agent_id,
      population_mgr_client:validation_complete(AgentId, Fitness),
      terminate(terminate_agent, State)
  end.

%%-----------------------------------------------------------------------------
%% @private
%% @doc This function is called to terminate the process. It performs
%%      any necessary cleaning up before exiting with the << Reason >>
%%      parameter that it was called with.
%% @end
%%-----------------------------------------------------------------------------
-spec terminate(atom(), state()) -> ok.
terminate(Reason, State) ->
  case Reason of
    normal ->
      ignore;
    _Other ->
      logr:debug({exoself, terminate, ok, "terminating phenotype", []}),
      terminate_phenotype(State)
  end,
  logr:debug({exoself, terminate, ok, undefined, [Reason]}),
  exit(Reason).

%%%============================================================================
%%% Internal functions
%%%============================================================================

handle(init_phase2, {AgentId, OpMode}) ->
  IdsNPids = ets:new(idsNpids, [set, private, {write_concurrency, true}, {read_concurrency, true}]),
  A = db:read(AgentId, agent),
  HeredityType = models:get(heredity_type, A),
  Cx = db:read(models:get(cx_id, A), cortex),
  [SIds, AIds, NIds] = models:get([sensor_ids, actuator_ids, neuron_ids], Cx),
  PrivateScapePids = spawn_scapes(SIds, AIds, AgentId, IdsNPids),
  CxPid = spawn_cortex(Cx, IdsNPids),
  spawn_sensors(SIds, IdsNPids),
  spawn_actuators(AIds, IdsNPids),
  spawn_neurons(NIds, HeredityType, IdsNPids),
  [CPPPids, CEPPids, SubstratePid] = set_encoding(A, SIds, AIds, IdsNPids),
  link_sensors(SIds, AgentId, OpMode, IdsNPids),
  link_actuators(AIds, AgentId, OpMode, IdsNPids),
  link_neurons(NIds, HeredityType, IdsNPids),
  {SPids, NPids, APids} = link_cortex(Cx, OpMode, IdsNPids),
  {TuningDurationFunction, Parameter} = models:get(tuning_duration_f, A),
  Morphology = models:get(morphology, models:get(constraint, A)),
  logr:debug({exoself, init2, ok, undefined, []}),
  #exoself_state{
    agent_id = AgentId,
    morphology = Morphology,
    generation = models:get(generation, A),
    ids_npids = IdsNPids,
    cx_pid = CxPid,
    specie_id = models:get(specie_id, A),
    spids = SPids,
    sids = SIds,
    npids = NPids,
    nids = NIds,
    apids = APids,
    aids = AIds,
    substrate_pid = SubstratePid,
    cpp_pids = CPPPids,
    cep_pids = CEPPids,
    private_scape_pids = PrivateScapePids,
    max_attempts = tuning_duration:TuningDurationFunction(Parameter, NIds,
      models:get(generation, A)),
    tuning_selection_f = models:get(tuning_selection_f, A),
    annealing_parameter = models:get(annealing_parameter, A),
    tuning_duration_f = models:get(tuning_duration_f, A),
    perturbation_range = models:get(perturbation_range, A),
    op_mode = OpMode
  };
handle(evaluation_complete, {S, CxPid, Fitness, Cycles, Time, GoalReached}) ->
  IdsNPids = S#exoself_state.ids_npids,
  HFitness = highest_fitness(S#exoself_state.highest_fitness, Fitness),
  {UHFitness, UAttempt} = updated_highest_fitness(S, Fitness, HFitness, IdsNPids),
  [neuron:reset_prep(Pid, self()) || Pid <- S#exoself_state.npids],
  gather_acks(length(S#exoself_state.npids)),
  [Pid ! {self(), reset} || Pid <- S#exoself_state.npids],
  maybe_reset_substrate(S#exoself_state.substrate_pid),
  UCycleAcc = S#exoself_state.cycle_acc + Cycles,
  UTimeAcc = S#exoself_state.time_acc + Time,
  UEvalAcc = S#exoself_state.eval_acc + 1,
  SpecieId = S#exoself_state.specie_id,
  population_mgr_client:set_evaluations(SpecieId, 1, Cycles, Time),
  case (UAttempt >= S#exoself_state.max_attempts) or (GoalReached == true) of
    true ->
      logr:debug({exoself, evaluation, ok, "ending training", []}),
      do_end_training(S, GoalReached, UHFitness, UEvalAcc, UCycleAcc, UTimeAcc),
      terminate_agent;
    false ->
      logr:debug({exoself, evaluation, ok, "continuing training", []}),
      ChosenNIdPs = do_continue_training(S, CxPid, IdsNPids),
      NewState = S#exoself_state{
        cycle_acc = UCycleAcc,
        time_acc = UTimeAcc,
        eval_acc = UEvalAcc,
        attempt = UAttempt,
        highest_fitness = UHFitness,
        perturbed_idps = ChosenNIdPs
      },
      NewState
  end.

do_continue_training(S, CxPid, IdsNPids) ->
  reenter_public_scape(
    [db:read(ets:lookup_element(IdsNPids, Id, 2), sensor) || Id <- S#exoself_state.spids],
    [db:read(ets:lookup_element(IdsNPids, Id, 2), actuator) || Id <- S#exoself_state.apids],
    S#exoself_state.morphology, S#exoself_state.agent_id),
  TuningSelectionFunction = S#exoself_state.tuning_selection_f,
  ChosenNIdPs = tuning_selection:TuningSelectionFunction(S#exoself_state.nids,
    S#exoself_state.generation, S#exoself_state.perturbation_range,
    S#exoself_state.annealing_parameter),
  [neuron:weight_perturb(ets:lookup_element(IdsNPids, NId, 2), self(), Spread) || {NId, Spread} <-
  ChosenNIdPs],
  cortex:reactivate(CxPid, self()),
  ChosenNIdPs.

do_end_training(S, GoalReached, UHFitness, UEvalAcc, UCycleAcc, UTimeAcc) ->
  case GoalReached of
    true ->
      logr:info({exoself, status, ok, "training_complete - goal reached",
        [{fitness, UHFitness}, {evaluations, UEvalAcc}, {cycles, UCycleAcc}, {time, UTimeAcc}]}),
      population_mgr_client:set_goal_reached();
    _ ->
      logr:info({exoself, status, ok, "training_complete - tuning complete",
        [{fitness, UHFitness}, {evaluations, UEvalAcc}, {cycles, UCycleAcc}, {time, UTimeAcc}]})
  end,
  terminate_phenotype(S),
  population_mgr_client:agent_terminated(S#exoself_state.agent_id).

highest_fitness(HighestFitness, Fitness) ->
 case HighestFitness of
   undefined ->
     [Val - 1 || Val <- Fitness];
   HiFi ->
     HiFi
 end.

updated_highest_fitness(S, Fitness, HFitness, IdsNPids) ->
 case utils:vec1_dominates_vec2(Fitness, HFitness, ?MIN_PIMPROVEMENT) of
   true ->
     true_vec1_dominates_vec2(S, Fitness, IdsNPids);
   false ->
     false_vec1_dominates_vec2(S, HFitness, IdsNPids)
 end.

maybe_reset_substrate(SubstratePid) ->
  case SubstratePid of
    undefined ->
      ok;
    SubstratePid ->
      substrate:reset_substrate(SubstratePid, self()),
      receive
        {SubstratePid, ready} ->
          ok
        after 3000 ->
          logr:warning({exoself, msg, error, "substrate reset message not received", []})
      end
  end.

%%-----------------------------------------------------------------------------
%% @private
%% @doc The spawn_scapes function first extracts all the scapes that the
%%      sensors and actuators interface with, it then creates a filtered scape
%%      list which only holds unique scape records, after which it further
%%      only selects those scapes that are private, and spawns them.
%% @end
%%-----------------------------------------------------------------------------
spawn_scapes(SensorIds, ActuatorIds, AgentId, IdsNPids) ->
  SensorScapes = [models:get(scape, db:read(Id, sensor)) || Id <- SensorIds],
  ActuatorScapes = [models:get(scape, db:read(Id, actuator)) || Id <- ActuatorIds],
  UniqueScapes = SensorScapes ++ (ActuatorScapes -- SensorScapes),
  PrivateSNTuples = spawn_private_scapes(AgentId, UniqueScapes, []),
  [ets:insert(IdsNPids, {ScapeName, Pid}) || {Pid, ScapeName} <- PrivateSNTuples],
  [ets:insert(IdsNPids, {Pid, ScapeName}) || {Pid, ScapeName} <- PrivateSNTuples],
  enter_public_scape(SensorIds, ActuatorIds, AgentId),
  PrivateScapePids = [Pid || {Pid, _ScapeName} <- PrivateSNTuples],
  PrivateScapePids.

spawn_private_scapes(AgentId, [{private, ScapeName} | UniqueScapes], Acc) ->
  {ok, Pid} = private_scape_sup:start_scape(AgentId, ScapeName),
  spawn_private_scapes(AgentId, UniqueScapes, [{Pid, ScapeName} | Acc]);
spawn_private_scapes(_AgentId, [], Acc) ->
  lists:reverse(Acc);
spawn_private_scapes(AgentId, [_ | _UniqueScapes], Acc) ->
  spawn_private_scapes(AgentId, [], Acc).

enter_public_scape(SensorIds, ActuatorIds, AgentId) ->
  A = db:read(AgentId, agent),
  Sensors = [db:read(Id, sensor) || Id <- SensorIds],
  Actuators = [db:read(Id, actuator) || Id <- ActuatorIds],
  Morphology = models:get(morphology, models:get(constraint, A)),
  enter_scape(Morphology, Sensors, Actuators, AgentId).

reenter_public_scape(Sensors, Actuators, Morphology, AgentId) ->
  enter_scape(Morphology, Sensors, Actuators, AgentId).

enter_scape(Morphology, Sensors, Actuators, AgentId) ->
  scape_mgr_client:enter(AgentId, [{Morphology, Sensors, Actuators}]).

%%-----------------------------------------------------------------------------
%% @private
%% @doc The spawn_sensors function spawns the sensors.
%% @end
%%-----------------------------------------------------------------------------
spawn_sensors([SId | SensorIds], IdsNPids) ->
  Pid = sensor:start(node(), self()),
  ets:insert(IdsNPids, {SId, Pid}),
  ets:insert(IdsNPids, {Pid, SId}),
  spawn_sensors(SensorIds, IdsNPids);
spawn_sensors([], IdsNPids) ->
  ets:insert(IdsNPids, {bias, bias}).

%%-----------------------------------------------------------------------------
%% @private
%% @doc The spawn_actuators function spawns the actuators.
%% @end
%%-----------------------------------------------------------------------------
spawn_actuators([AId | ActuatorIds], IdsNPids) ->
  Pid = actuator:start(node(), self()),
  ets:insert(IdsNPids, {AId, Pid}),
  ets:insert(IdsNPids, {Pid, AId}),
  spawn_actuators(ActuatorIds, IdsNPids);
spawn_actuators([], IdsNPids) ->
  ets:insert(IdsNPids, {bias, bias}).

%%-----------------------------------------------------------------------------
%% @private
%% @doc The spawn_neurons function spawns the neurons.
%% @end
%%-----------------------------------------------------------------------------
spawn_neurons([NId | NeuronIds], HeredityType, IdsNPids) ->
  Pid = neuron:start(node(), self()),
  ets:insert(IdsNPids, {NId, Pid}),
  ets:insert(IdsNPids, {Pid, NId}),
  spawn_neurons(NeuronIds, HeredityType, IdsNPids);
spawn_neurons([], _HeredityType, IdsNPids) ->
  ets:insert(IdsNPids, {bias, bias}).

%%-----------------------------------------------------------------------------
%% @private
%% @doc The spawn_cortex function spawns the cortex process.
%% @end
%%-----------------------------------------------------------------------------
spawn_cortex(Cx, IdsNPids) ->
  CxId = models:get(id, Cx),
  CxPid = cortex:start(node(), self()),
  ets:insert(IdsNPids, {CxId, CxPid}),
  ets:insert(IdsNPids, {CxPid, CxId}),
  CxPid.

%%-----------------------------------------------------------------------------
%% @private
%% @doc The spawn_substrate_cpps function spawns the actuators.
%% @end
%%-----------------------------------------------------------------------------
spawn_substrate_cpps([CPPId | CPPIds], IdsNPids) ->
  Pid = substrate_cpp:start(node(), self()),
  ets:insert(IdsNPids, {CPPId, Pid}),
  ets:insert(IdsNPids, {Pid, CPPId}),
  spawn_substrate_cpps(CPPIds, IdsNPids);
spawn_substrate_cpps([], IdsNPids) ->
  ets:insert(IdsNPids, {bias, bias}).

%%-----------------------------------------------------------------------------
%% @private
%% @doc The spawn_substrate_ceps function spawns the actuators.
%% @end
%%-----------------------------------------------------------------------------
spawn_substrate_ceps([CEPId | CEPIds], IdsNPids) ->
  Pid = substrate_cep:start(node(), self()),
  ets:insert(IdsNPids, {CEPId, Pid}),
  ets:insert(IdsNPids, {Pid, CEPId}),
  spawn_substrate_ceps(CEPIds, IdsNPids);
spawn_substrate_ceps([], IdsNPids) ->
  ets:insert(IdsNPids, {bias, bias}).

%%-----------------------------------------------------------------------------
%% @private
%% @doc The spawn_substrate function spawns the substrate process.
%% @end
%%-----------------------------------------------------------------------------
spawn_substrate(SubstrateId, IdsNPids) ->
  SubstratePid = substrate:start(node(), self()),
  ets:insert(IdsNPids, {SubstrateId, SubstratePid}),
  ets:insert(IdsNPids, {SubstratePid, SubstrateId}),
  SubstratePid.

set_encoding(A, SIds, AIds, IdsNPids) ->
  case models:get(encoding_type, A) of
    substrate ->
      SubstrateId = models:get(substrate_id, A),
      Substrate = db:read(SubstrateId, substrate),
      CPPIds = models:get(cpp_ids, Substrate),
      CEPIds = models:get(cep_ids, Substrate),
      spawn_substrate_cpps(CPPIds, IdsNPids),
      spawn_substrate_ceps(CEPIds, IdsNPids),
      SubstratePid = spawn_substrate(SubstrateId, IdsNPids),
      link_substrate_cpps(CPPIds, SubstratePid, IdsNPids),
      link_substrate_ceps(CEPIds, SubstratePid, IdsNPids),
      CPPPids = [ets:lookup_element(IdsNPids, Id, 2) || Id <- CPPIds],
      CEPPids = [ets:lookup_element(IdsNPids, Id, 2) || Id <- CEPIds],
      link_substrate(Substrate, SubstratePid, SIds, AIds, CPPPids, CEPPids, IdsNPids),
      [CPPPids, CEPPids, SubstratePid];
    _ ->
      [[], [], undefined]
  end.

%%-----------------------------------------------------------------------------
%% @private
%% @doc The link_sensors function activates the sensors with their
%%      initialization parameters, composed of the Pid lists and other
%%      information which are needed by the sensors to link up and interface
%%      with other elements in the distributed phenotype.
%% @end
%%-----------------------------------------------------------------------------
link_sensors([SId | SensorIds], AgentId, OpMode, IdsNPids) ->
  S = db:read(SId, sensor),
  SPid = ets:lookup_element(IdsNPids, SId, 2),
  CxPid = ets:lookup_element(IdsNPids, models:get(cx_id, S), 2),
  SName = models:get(name, S),
  FanoutIds = models:get(fanout_ids, S),
  FanoutPids = [ets:lookup_element(IdsNPids, Id, 2) || Id <- FanoutIds],
  Scape = case models:get(scape, S) of
    {private, ScapeName} ->
      ets:lookup_element(IdsNPids, ScapeName, 2);
    {public, ScapeName} ->
      ScapeName
  end,
  sensor:init_phase2(SPid, self(), SId, AgentId, CxPid, Scape, SName, models:get(vl, S),
    models:get(parameters, S), FanoutPids, OpMode),
  link_sensors(SensorIds, AgentId, OpMode, IdsNPids);
link_sensors([], _AgentId, _OpMode, _IdsNPids) ->
  ok.

%%-----------------------------------------------------------------------------
%% @private
%% @doc The link_actuators function activates the actuators with
%%      their initialization parameters, composed of the Pid lists and other
%%      information which are needed by the actuators to link up and interface
%%      with other elements in the distributed phenotype.
%% @end
%%-----------------------------------------------------------------------------
link_actuators([AId | ActuatorIds], AgentId, OpMode, IdsNPids) ->
  A = db:read(AId, actuator),
  APid = ets:lookup_element(IdsNPids, AId, 2),
  CxPid = ets:lookup_element(IdsNPids, models:get(cx_id, A), 2),
  AName = models:get(name, A),
  FaninIds = models:get(fanin_ids, A),
  FaninPids = [ets:lookup_element(IdsNPids, Id, 2) || Id <- FaninIds],
  Scape = case models:get(scape, A) of
    {private, ScapeName} ->
      ets:lookup_element(IdsNPids, ScapeName, 2);
    {public, ScapeName} ->
      ScapeName
  end,
  actuator:init_phase2(APid, self(), AId, AgentId, CxPid, Scape, AName, models:get(vl, A),
    models:get(parameters, A), FaninPids, OpMode),
  link_actuators(ActuatorIds, AgentId, OpMode, IdsNPids);
link_actuators([], _AgentId, _OpMode, _IdsNPids) ->
  ok.

%%-----------------------------------------------------------------------------
%% @private
%% @doc The link_substrate_cpps function activates the substrate_cpps with
%%      their initialization parameters, composed of the Pid lists and other
%%      information which are needed by the substrate_cpps to link up and
%%      interface with other elements in the distributed phenotype.
%% @end
%%-----------------------------------------------------------------------------
link_substrate_cpps([CPPId | CPPIds], SubstratePid, IdsNPids) ->
  CPP = db:read(CPPId, sensor),
  CPPPid = ets:lookup_element(IdsNPids, CPPId, 2),
  CxPid = ets:lookup_element(IdsNPids, models:get(cx_id, CPP), 2),
  CPPName = models:get(name, CPP),
  FanoutIds = models:get(fanout_ids, CPP),
  FanoutPids = [ets:lookup_element(IdsNPids, Id, 2) || Id <- FanoutIds],
  substrate_cpp:init_phase2(CPPPid, self(), CPPId, CxPid, SubstratePid,
    CPPName, models:get(vl, CPP), models:get(parameters, CPP), FanoutPids),
  link_substrate_cpps(CPPIds, SubstratePid, IdsNPids);
link_substrate_cpps([], _SubstratePid, _IdsNPids) ->
  ok.

%%-----------------------------------------------------------------------------
%% @private
%% @doc The link_substrate_ceps function activates the substrate_ceps with
%%      their initialization parameters, composed of the Pid lists and other
%%      information which are needed by the substrate_ceps to link up and
%%      interface with other elements in the distributed phenotype.
%% @end
%%-----------------------------------------------------------------------------
link_substrate_ceps([CEPId | CEPIds], SubstratePid, IdsNPids) ->
  CEP = db:read(CEPId, actuator),
  CEPPid = ets:lookup_element(IdsNPids, CEPId, 2),
  CxPid = ets:lookup_element(IdsNPids, models:get(cx_id, CEP), 2),
  CEPName = models:get(name, CEP),
  FaninIds = models:get(fanin_ids, CEP),
  FaninPids = [ets:lookup_element(IdsNPids, Id, 2) || Id <- FaninIds],
  substrate_cep:init_phase2(CEPPid, self(), CEPId, CxPid, SubstratePid, CEPName,
    models:get(parameters, CEP), FaninPids),
  link_substrate_ceps(CEPIds, SubstratePid, IdsNPids);
link_substrate_ceps([], _SubstratePid, _IdsNPids) ->
  ok.

%%-----------------------------------------------------------------------------
%% @private
%% @doc The link_substrate function activates the substrate with its
%%      initialization parameters.
%% @end
%%-----------------------------------------------------------------------------
link_substrate(Substrate, SubstratePid, SIds, AIds, CPPPids, CEPPids, IdsNPids) ->
  SDensities = models:get(densities, Substrate),
  SPlasticity = models:get(plasticity, Substrate),
  SLinkform = models:get(linkform, Substrate),
  Sensors = [db:read(SId, sensor) || SId <- SIds],
  Actuators = [db:read(AId, actuator) || AId <- AIds],
  substrate:init_phase2(SubstratePid, self(), Sensors, Actuators,
    [ets:lookup_element(IdsNPids, Id, 2) || Id <- SIds],
    [ets:lookup_element(IdsNPids, Id, 2) || Id <- AIds],
    CPPPids, CEPPids, SDensities, SPlasticity, SLinkform).

%%-----------------------------------------------------------------------------
%% @private
%% @doc The link_neurons function activates the neurons with their
%%      initialization parameters, composed of the Pid lists and other
%%      information which are needed by the neurons to link up and interface
%%      with other elements in the distributed phenotype.
%% @end
%%-----------------------------------------------------------------------------
link_neurons([NId | NeuronIds], HeredityType, IdsNPids) ->
  N = db:read(NId, neuron),
  NPid = ets:lookup_element(IdsNPids, NId, 2),
  CxPid = ets:lookup_element(IdsNPids, models:get(cx_id, N), 2),
  AFName = models:get(af, N),
  PFName = models:get(pf, N),
  AggrFName = models:get(aggr_f, N),
  InputIdPs = models:get(input_idps, N),
  InputIdPsModulation = models:get(input_idps_modulation, N),
  OutputIds = models:get(output_ids, N),
  ROIds = models:get(ro_ids, N),
  SIPidPs = convert_idps2pidps(InputIdPs, [], IdsNPids),
  MIPidPs = convert_idps2pidps(InputIdPsModulation, [], IdsNPids),
  OPids = [ets:lookup_element(IdsNPids, Id, 2) || Id <- OutputIds],
  ROPids = [ets:lookup_element(IdsNPids, Id, 2) || Id <- ROIds],
  neuron:init_phase2(NPid, self(), NId, CxPid, AFName, PFName, AggrFName, HeredityType, SIPidPs,
    MIPidPs, OPids, ROPids),
  link_neurons(NeuronIds, HeredityType, IdsNPids);
link_neurons([], _HeredityType, _IdsNPids) ->
  ok.

%%-----------------------------------------------------------------------------
%% @private
%% @doc The link_cortex function activates the cortex with its
%%      initialization parameters.
%% @end
%%-----------------------------------------------------------------------------
link_cortex(Cx, OpMode, IdsNPids) ->
  CxId = models:get(id, Cx),
  CxPid = ets:lookup_element(IdsNPids, CxId, 2),
  SIds = models:get(sensor_ids, Cx),
  AIds = models:get(actuator_ids, Cx),
  NIds = models:get(neuron_ids, Cx),
  SPids = [ets:lookup_element(IdsNPids, SId, 2) || SId <- SIds],
  NPids = [ets:lookup_element(IdsNPids, NId, 2) || NId <- NIds],
  APids = [ets:lookup_element(IdsNPids, AId, 2) || AId <- AIds],
  cortex:init_phase2(CxPid, self(), CxId, SPids, NPids, APids, OpMode),
  {SPids, NPids, APids}.

true_vec1_dominates_vec2(S, Fitness, IdsNPids) ->
  [neuron:weight_backup(NPid, self()) || NPid <- S#exoself_state.npids],
  A = db:read(S#exoself_state.agent_id, agent),
  [MainFitness | _] = Fitness,
  UA = models:set([{fitness, Fitness}, {main_fitness, MainFitness}], A),
  db:write(UA, agent),
  backup_genotype(S#exoself_state.spids, S#exoself_state.npids,
    S#exoself_state.apids, IdsNPids),
  {Fitness, 0}.

false_vec1_dominates_vec2(S, HFitness, IdsNPids) ->
  PerturbedIdPs = S#exoself_state.perturbed_idps,
  [neuron:weight_restore(ets:lookup_element(IdsNPids, Id, 2), self())
  || {Id, _Spread} <- PerturbedIdPs],
  {HFitness, S#exoself_state.attempt + 1}.

%%-----------------------------------------------------------------------------
%% @private
%% @doc The backup_genotype uses get_backup to contact all the neurons in its
%%      NN and request for the neurons Ids and their InputIdPs. Once the
%%      updated InputIdPs from all the neurons have been accumulated, they
%%      are passed through the update_genotype function to produce updated
%%      neurons, and write them to database.
%% @end
%%-----------------------------------------------------------------------------
backup_genotype(_SPids, NPids, _APids, IdsNPids) ->
  NeuronIdsNWeights = get_backup(NPids, []),
  update_genotype(NeuronIdsNWeights, IdsNPids).

%%-----------------------------------------------------------------------------
%% @private
%% @doc For every {NId, PidPs} tuple the update_genotype function extracts the
%%      neuron with the id: NId, updates the neurons inputIdPs, and writes
%%      the updated neuron to database.
%% @end
%%-----------------------------------------------------------------------------
update_genotype([{NId, SIPidPs, MIPidPs, PF} | WeightPs], IdsNPids) ->
  N = db:read(NId, neuron),
  USIIdPs = lookup_idps(SIPidPs, IdsNPids),
  UMIIdPs = lookup_idps(MIPidPs, IdsNPids),
  UN = models:set([{input_idps, USIIdPs}, {input_idps_modulation, UMIIdPs}, {pf, PF}], N),
  db:write(UN, neuron),
  update_genotype(WeightPs, IdsNPids);
update_genotype([], _IdsNPids) ->
  ok.

lookup_idps(IPidPs, IdsNPids) ->
  [{ets:lookup_element(IdsNPids, Pid, 2), WeightsP} || {Pid, WeightsP} <- IPidPs].

%%-----------------------------------------------------------------------------
%% @private
%% @doc The backup_genotype uses get_backup/2 to contact all the neurons in
%%      its NN and request for the neuron's Ids and their Input_IdPs. Once the
%%      updated Input_IdPs from all the neurons have been accumulated, they are
%%      passed through the update_genotype/2 function to produce updated
%%      neurons, and write them to database.
%% @end
%%-----------------------------------------------------------------------------
get_backup([NPid | NPids], Acc) ->
  neuron:get_backup(NPid, self()),
  receive
    {NPid, NId, SWeightTuples, MWeightTuples, PF} ->
      get_backup(NPids, [{NId, SWeightTuples, MWeightTuples, PF} | Acc])
  end;
get_backup([], Acc) ->
  Acc.

%%-----------------------------------------------------------------------------
%% @private
%% @doc The terminate_phenotype function terminates sensors, actuators,
%%      neurons, all private scapes, and the cortex which composes the NN
%%      based system.
%% @end
%%-----------------------------------------------------------------------------
terminate_phenotype(S) ->
  CxPid = S#exoself_state.cx_pid,
  SPids = S#exoself_state.spids,
  NPids = S#exoself_state.npids,
  APids = S#exoself_state.apids,
  CPPPids = S#exoself_state.cpp_pids,
  CEPPids = S#exoself_state.cep_pids,
  ScapePids = S#exoself_state.private_scape_pids,
  [sensor:stop(SPid, self()) || SPid <- SPids],
  [actuator:stop(APid, self()) || APid <- APids],
  [neuron:stop(NPid, self()) || NPid <- NPids],
  [private_scape_sup:stop_scape(Pid) || Pid <- ScapePids],
  [substrate_cpp:stop(Pid, self()) || Pid <- CPPPids],
  [substrate_cep:stop(Pid, self()) || Pid <- CEPPids],
  case S#exoself_state.substrate_pid of
    undefined ->
      ok;
    SubstratePid ->
      substrate:stop(SubstratePid, self())
  end,
  cortex:stop(CxPid, self()).

%%-----------------------------------------------------------------------------
%% @private
%% @doc The convert_idps2pidps converts the IdPs tuples into tuples that use
%%      Pids instead of Ids, such that the Neuron will know which weights are
%%      to be associated with which incoming vector signals. The last element
%%      is the bias, which is added to the list in a non tuple form.
%%      Afterwards, the list is reversed to take its proper order.
%% @end
%%-----------------------------------------------------------------------------
convert_idps2pidps([{Id, WeightsP} | FaninIdPs], Acc, IdsNPids) ->
  convert_idps2pidps(FaninIdPs, [{ets:lookup_element(IdsNPids, Id, 2), WeightsP} | Acc], IdsNPids);
convert_idps2pidps([], Acc, _IdsNPids) ->
  lists:reverse(Acc).

%%-----------------------------------------------------------------------------
%% @private
%% @doc The gather_acks ensures that the X number of {From,ready} messages
%%      are sent to it, before it returns with done. X is set by the caller of
%%      the function.
%% @end
%%-----------------------------------------------------------------------------
gather_acks(0) ->
  done;
gather_acks(PidIndex) ->
  receive
    {_From, ready} ->
      gather_acks(PidIndex - 1)
    after 30000 ->
      logr:warning({exoself, msg, error, "not all acks received", [PidIndex]})
  end.