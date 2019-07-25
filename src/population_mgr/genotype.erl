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
%%% @doc The genotype module encapsulates the NN based system creation and NN
%%%      genotype access and storage. Unlike in static NN based systems,
%%%      topology and weight evolving artificial neural network systems
%%%      (TWEANNs) can modify the very topology and structure of a NN. We do
%%%      not need to figure out what NN topology we should give to our NN
%%%      system, because it will evolve the topology most optimal for the
%%%      problem we give it. Plus, we never really know ahead of time what the
%%%      most optimal NN topology needed to solve some particular problem
%%%      anyway. The seed NN genotype should be the simplest possible, given
%%%      the particular morphology of the agent, we let the neuroevolutionary
%%%      process complexify the topology of the NN system over time.
%%%      Finally, because we use different kinds of activation functions, not
%%%      only tanh but also sin, abs, sgn...we might wish for some species in
%%%      the population to be started with a particular subset of these
%%%      activation functions, and other species with another subset, to
%%%      perhaps observe how and which evolutionary paths they take due to
%%%      these different constraints. For this reason, we also implement a
%%%      constraint record which the population_mgr can use when
%%%      constructing agents. The constraint record specifies which morphology
%%%      and which set of activation functions the seed agent and its
%%%      offspring should have access to during evolution.
%%% @end
%%%----------------------------------------------------------------------------
-module(genotype).

-include_lib("kernel/include/logger.hrl").

%% API
-export([
  construct_agent/3,
  update_fingerprint/1,
  clone_agent/1,
  delete_agent/1,
  unique_id/0,
  construct_neuron/6,
  link_neuron/4,
  create_neural_weights_p/3,
  print/1
]).

%% Xref
-ignore_xref([
  print/1
]).

%%%============================================================================
%%% API
%%%============================================================================

%%-----------------------------------------------------------------------------
%% @doc The population mgr should have all the information with regards
%%      to the morphologies and specie constraint under which the agent's
%%      genotype should be created. Thus the construct_agent/3 is run with
%%      the SpecieId to which this NN based system will belong, the AgentId
%%      that this NN based intelligent agent will have, and the SpecCon
%%      (specie constraint) that will define the list of activation functions
%%      and other parameters from which the seed agent can choose its
%%      parameters. First the generation is set to 0, since the agent is just
%%      created, then the construct_cortex/3 is ran, which creates the NN and
%%      returns its CxId. Once the NN is created and the the cortex's id is
%%      returned, we can fill out the information needed by the agent record,
%%      and write it to the database.
%% @end
%%-----------------------------------------------------------------------------
-spec construct_agent({specie, float()}, {agent, float()}, models:constraint()) -> ok.
construct_agent(SpecieId, AgentId, SpecC) ->
  utils:random_seed(),
  Generation = 0,
  EncodingType = random_element(models:get(agent_encoding_types, SpecC)),
  SPlasticity = random_element(models:get(substrate_plasticities, SpecC)),
  SLinkform = random_element(models:get(substrate_linkforms, SpecC)),
  {CxId, Pattern, SubstrateId} = construct_cortex(AgentId, Generation,
                                                  SpecC, EncodingType,
                                                  SPlasticity, SLinkform),
  Agent = models:agent(#{
    id => AgentId,
    encoding_type => EncodingType,
    generation => Generation,
    population_id => undefined,
    specie_id => SpecieId,
    cx_id => CxId,
    fingerprint => undefined,
    constraint => SpecC,
    evo_hist => [],
    fitness => 0.0,
    innovation_factor => 0,
    pattern => Pattern,
    tuning_selection_f => random_element(models:get(tuning_selection_fs, SpecC)),
    annealing_parameter => random_element(models:get(annealing_parameters, SpecC)),
    tuning_duration_f => models:get(tuning_duration_f, SpecC),
    perturbation_range => random_element(models:get(perturbation_ranges, SpecC)),
    perturbation_qty => multiple,
    mutation_operators => models:get(mutation_operators, SpecC),
    tot_topological_mutations_f => random_element(models:get(tot_topological_mutations_fs, SpecC)),
    heredity_type => random_element(models:get(heredity_types, SpecC)),
    substrate_id => SubstrateId,
    offspring_ids => [],
    parent_ids => [],
    champion_flag => false,
    fs => 1.0,
    main_fitness => undefined
  }),
  db:write(Agent, agent),
  update_fingerprint(AgentId).

%%-----------------------------------------------------------------------------
%% @doc The update_fingerprint calculates the fingerprint of the agent,
%%      where the fingerprint is just a tuple of the various general
%%      features of the NN based system, a list of features that play some
%%      role in distinguishing its genotype's general properties from those
%%      of other NN systems. The fingerprint here is composed of the
%%      generalized pattern (pattern minus the unique ids), generalized
%%      evolutionary history (evolutionary history minus the unique ids of
%%      the elements), a generalized sensor set, and a generalized actuator
%%      set.
%% @end
%%-----------------------------------------------------------------------------
-spec update_fingerprint(models:agent_id()) -> ok.
update_fingerprint(AgentId) ->
  A = db:read(AgentId, agent),
  Cx = db:read(models:get(cx_id, A), cortex),
  SIds = models:get(sensor_ids, Cx),
  AIds = models:get(actuator_ids, Cx),
  GeneralizedSensors = [models:set([{id, undefined}, {cx_id, undefined},
    {fanout_ids, []}, {generation, undefined}], db:read(SId, sensor)) || SId <- SIds],
  GeneralizedActuators = [models:set([{id, undefined}, {cx_id, undefined},
    {fanin_ids, []}, {generation, undefined}], db:read(AId, actuator)) || AId <- AIds],
  GeneralizedPattern = [{LayerIndex, length(LNIds)}||{LayerIndex, LNIds} <- models:get(pattern, A)],
  GeneralizedEvoHist = generalize_evo_hist(models:get(evo_hist, A), []),
  NIds = models:get(neuron_ids, Cx),
  Type = models:get(encoding_type, A),
  {TotNeuronILs, TotNeuronOLs, TotNeuronROs, AFDistribution} = get_node_summary(NIds),
  TopologySummary = models:topology_summary(#{
    type => Type,
    tot_neurons => length(NIds),
    tot_n_ils => TotNeuronILs,
    tot_n_ols => TotNeuronOLs,
    tot_n_ros => TotNeuronROs,
    af_distribution => AFDistribution
  }),
  Fingerprint = {GeneralizedPattern, GeneralizedEvoHist, GeneralizedSensors,
  GeneralizedActuators, TopologySummary},
  db:write(models:set({fingerprint, Fingerprint}, A), agent).

%%-----------------------------------------------------------------------------
%% @doc The clone_agent accepts AgentId and generates a CloneAgentId. It then
%%      calls clone_agent which accepts AgentId, and CloneAgentId, and then
%%      clones the agent, giving the clone CloneAgentId. The function first
%%      creates an ETS table to which it writes the ids of all the elements
%%      of the genotype, and their corresponding clone ids. Once all ids and
%%      clone ids have been generated, the function then begins to clone the
%%      actual elements. clone_agent/2 first clones the neurons using
%%      clone_neurons/2, then the sensors using clone_sensonrs/2, and
%%      finally the actuators using clone_actuators. Once these elements are
%%      cloned, the function writes to database the clone versions of the
%%      cortex and the agent records, by writing to database the original
%%      records with updated ids.
%% @end
%%-----------------------------------------------------------------------------
-spec clone_agent(models:agent_id()) -> {agent, float()}.
clone_agent(AgentId) ->
  CloneAgentId = {agent, unique_id()},
  clone_agent(AgentId, CloneAgentId).

%%-----------------------------------------------------------------------------
%% @doc The delete_agent accepts the id of an agent, and then deletes that
%%      agent's genotype. This function assumes that the id of the agent will
%%      be removed from the specie's agent_ids list, and any other clean up
%%      procedures, by the calling function.
%% @end
%%-----------------------------------------------------------------------------
-spec delete_agent(models:agent_id()) -> ok.
delete_agent(AgentId) ->
  A = db:read(AgentId, agent),
  Cx = db:read(models:get(cx_id, A), cortex),
  [db:delete(Id, neuron) || Id <- models:get(neuron_ids, Cx)],
  [db:delete(Id, sensor) || Id <- models:get(sensor_ids, Cx)],
  [db:delete(Id, actuator) || Id <- models:get(actuator_ids, Cx)],
  db:delete(models:get(cx_id, A), cortex),
  db:delete(AgentId, agent),
  case models:get(substrate_id, A) of
    undefined ->
      ok;
    SubstrateId ->
      Substrate = db:read(SubstrateId, substrate),
      [db:delete(Id, sensor) || Id <- models:get(cpp_ids, Substrate)],
      [db:delete(Id, actuator) || Id <- models:get(cep_ids, Substrate)],
      db:delete(SubstrateId, substrate)
  end.

%%-----------------------------------------------------------------------------
%% @doc The unique_id creates a unique Id, the
%%      Id is a floating point value. NOT cryptographically strong.
%% @end
%%-----------------------------------------------------------------------------
-spec unique_id() -> float().
unique_id() ->
  1 / rand:uniform() * 1000000 / 1000000.

%%-----------------------------------------------------------------------------
%% @doc Each neuron record is composed by the construct_neuron function. The
%%      construct_neuron creates the Input list from the tuples
%%      [{Id, Weights}...] using the vector lengths specified in the InputSpecs
%%      list. The create_input_idps function uses create_neural_weights_p to
%%      generate a tuple list with random weights in the range of -0.5 to 0.5,
%%      and plasticity parameters dependent on the PF function. The activation
%%      function that the neuron uses is chosen randomly from the neural_afs
%%      list within the constraint record passed to the construct_neuron
%%      function. construct_neuron uses calculate_roids to extract the list of
%%      recursive connection ids from the OutputIds passed to it. Once the
%%      neuron record is filled in, it is saved to the database.
%% @end
%%-----------------------------------------------------------------------------
-spec construct_neuron({cortex, {origin, float()}}, non_neg_integer(), models:constraint(),
  {neuron, {float(), float()}}, [{models:neuron_ids(), float()}],
  [{actuator | neuron, {float(), float()}}]) -> ok.
construct_neuron(CxId, Generation, SpecCon, NId, InputSpecs, OutputIds) ->
  PF = {PFName, _NLParameters} = generate_neuron_pf(models:get(neural_pfns, SpecCon)),
  AF = generate_neuron_af(models:get(neural_afs, SpecCon)),
  InputIdPs = create_input_idps(PFName, InputSpecs, []),
  Neuron = models:neuron(#{
    id => NId,
    generation => Generation,
    cx_id => CxId,
    af => AF,
    pf => PF,
    aggr_f => generate_neuron_aggr_f(models:get(neural_aggr_fs, SpecCon)),
    input_idps => InputIdPs,
    input_idps_modulation => [],
    output_ids => OutputIds,
    ro_ids => calculate_roids(NId, OutputIds, [])
  }),
  db:write(Neuron, neuron).

%%-----------------------------------------------------------------------------
%% @doc The link_neuron function links the neuron to another element. For
%%      example, to another neuron.
%% @end
%%-----------------------------------------------------------------------------
-spec link_neuron(integer(), [models:sensor_id() | models:neuron_id()],
  models:neuron_id(), [models:actuator_id() | models:neuron_id()]) -> [ok].
link_neuron(Generation, FromIds, NId, ToIds) ->
  [genome_mutator:link_from_element_to_element(Generation, FromId, NId) || FromId <- FromIds],
  [genome_mutator:link_from_element_to_element(Generation, NId, ToId) || ToId <- ToIds].

%%-----------------------------------------------------------------------------
%% @doc Each neuron record is composed by the construct_neuron function.
%%      The construct_neuron creates the Input list from the tuples
%%      [{Id, Weights}...] using the vector lengths specified in the
%%      InputSpecs list. The create_input_idps function uses
%%      create_neural_weights_p to generate a tuple list with random weights
%%      in the range of -0.5 to 0.5, and plasticity parameters dependent on
%%      the PF function. The activation function that the neuron uses is
%%      chosen randomly from the neural_afs list within the constraint record
%%      passed to the construct_neuron function. construct_neuron uses
%%      calculate_roids to extract the list of recursive connection ids
%%      from the OutputIds passed to it. Once the neuron record is filled
%%      in, it is saved to the database.
%% @end
%%-----------------------------------------------------------------------------
-spec create_neural_weights_p(atom(), non_neg_integer(), [float()]) -> [{float(), [float()] | []}].
create_neural_weights_p(_PFName, 0, Acc) ->
  Acc;
create_neural_weights_p(PFName, Index, Acc) ->
  W = rand:uniform() - 0.5,
 create_neural_weights_p(PFName, Index - 1, [{W, plasticity:PFName(weight_parameters)} | Acc]).

%%%============================================================================
%%% Internal functions
%%%============================================================================

%%-----------------------------------------------------------------------------
%% @doc The construct_cortex generates a new CxId, extracts the morphology
%%      from the Constraint record passed to it in SpecCon, and then extracts
%%      the initial sensors and actuators for that morphology. After the
%%      sensors and actuators are extracted, the function calls
%%      construct_InitialNeuroLayer, which creates a single layer of neurons
%%      connected to the specified sensors and actuators, and returns the ids
%%      of the created neurons. Finally, the sensors and actuator ids are
%%      extracted from the sensors and actuators, and the cortex record is
%%      composed and stored to the database.
%% @end
%%-----------------------------------------------------------------------------
construct_cortex(AgentId, Generation, SpecCon, EncodingType, SPlasticity, SLinkform) ->
  CxId = {cortex, {origin, unique_id()}},
  Morphology = models:get(morphology, SpecCon),
  {Cortex, Pattern, SubstrateId} = case EncodingType of
    neural ->
      construct_cortex_neural_encoded(AgentId, Generation, SpecCon, CxId, Morphology);
    substrate ->
      construct_cortex_substrate_encoded(AgentId, Generation, SpecCon, CxId, Morphology,
        SPlasticity, SLinkform)
  end,
  db:write(Cortex, cortex),
  {CxId, Pattern, SubstrateId}.

construct_cortex_neural_encoded(AgentId, Generation, SpecCon, CxId, Morphology) ->
  Sensors =
  [models:set([{id, {sensor, {-1.0, unique_id()}}}, {cx_id, CxId}, {generation, Generation}], S)
  || S <- morphology:get_init_sensors(Morphology)],
  Actuators =
  [models:set([{id, {actuator, {1.0, unique_id()}}}, {cx_id, CxId}, {generation, Generation}], A)
  || A <- morphology:get_init_actuators(Morphology)],
  [db:write(S, sensor) || S <- Sensors],
  [db:write(A, actuator) || A <- Actuators],
  {NIds, Pattern} = construct_seed_nn(CxId, Generation, SpecCon, Sensors, Actuators, []),
  SIds = [models:get(id, S) || S <- Sensors],
  AIds = [models:get(id, A) || A <- Actuators],
  Cortex = models:cortex(#{
    id => CxId,
    agent_id => AgentId,
    neuron_ids => NIds,
    sensor_ids => SIds,
    actuator_ids => AIds
  }),
  SubstrateId = undefined,
  {Cortex, Pattern, SubstrateId}.

construct_cortex_substrate_encoded(AgentId, Generation, SpecCon, CxId, Morphology,
  SPlasticity, SLinkform) ->
  SubstrateId = {substrate, {void, unique_id()}},
  Sensors =
  [models:set([{id, {sensor, {-1.0, unique_id()}}},
    {cx_id, CxId}, {generation, Generation}, {fanout_ids, [SubstrateId]}], S)
  || S <- morphology:get_init_sensors(Morphology)],
  Actuators = [models:set([{id, {actuator, {1.0, unique_id()}}},
    {cx_id, CxId}, {generation, Generation}, {fanin_ids, [SubstrateId]}], A)
  || A <- morphology:get_init_actuators(Morphology)],
  [db:write(S, sensor) || S <- Sensors],
  [db:write(A, actuator) || A <- Actuators],
  Dimensions = calculate_optimal_substrate_dimension(Sensors, Actuators),
  Density = 5,
  Depth = 1,
  Densities = [Depth, 1 | lists:duplicate(Dimensions - 2, Density)],
  SubstrateCPPs =
  [models:set([{id, {sensor, {-1.0, unique_id()}}}, {cx_id, CxId}, {generation, Generation}], CPP)
  || CPP <- morphology:get_init_substrate_cpps(Dimensions, SPlasticity)],
  SubstrateCEPs =
  [models:set([{id, {actuator, {1.0, unique_id()}}}, {cx_id, CxId}, {generation, Generation}], CEP)
  || CEP <- morphology:get_init_substrate_ceps(Dimensions, SPlasticity)],
  [db:write(SubstrateCPP, sensor) || SubstrateCPP <- SubstrateCPPs],
  [db:write(SubstrateCEP, actuator) || SubstrateCEP <- SubstrateCEPs],
  {NIds, Pattern} = construct_seed_nn(CxId, Generation, SpecCon, SubstrateCPPs, SubstrateCEPs, []),
  SIds = [models:get(id, S) || S <- Sensors],
  AIds = [models:get(id, A) || A <- Actuators],
  CPPIds = [models:get(id, CPP) || CPP <- SubstrateCPPs],
  CEPIds = [models:get(id, CEP) || CEP <- SubstrateCEPs],
  Substrate =
  models:substrate(#{
    id => SubstrateId,
    agent_id => AgentId,
    densities => Densities,
    linkform => SLinkform,
    plasticity => SPlasticity,
    cpp_ids => CPPIds,
    cep_ids => CEPIds
  }),
  db:write(Substrate, substrate),
  Cortex = models:cortex(#{
    id => CxId,
    agent_id => AgentId,
    neuron_ids => NIds,
    sensor_ids => SIds,
    actuator_ids => AIds
  }),
  {Cortex, Pattern, SubstrateId}.

%%-----------------------------------------------------------------------------
%% @doc The random_element function accepts a list as input, and returns a
%%      single, randomly chosen element as output.
%% @end
%%-----------------------------------------------------------------------------
random_element(List) ->
  lists:nth(rand:uniform(length(List)), List).

%%-----------------------------------------------------------------------------
%% @doc construct_seed_nn results in a single layer connected directly from
%%      all sensors and to the set of actuators.
%% @end
%%-----------------------------------------------------------------------------
construct_seed_nn(CxId, Generation, SpecCon, Sensors, [A | Actuators], Acc) ->
  NIds = [{neuron, {0.0, genotype:unique_id()}} || _<- lists:seq(1, models:get(vl, A))],
  [construct_neuron(CxId, Generation, SpecCon, NId, [], []) || NId <- NIds],
  [link_neuron(Generation, [models:get(id, S) || S <- Sensors],
    NId, [models:get(id, A)]) || NId <- NIds],
  construct_seed_nn(CxId, Generation, SpecCon, Sensors, Actuators, lists:append(NIds, Acc));
construct_seed_nn(_CxId, _Generation, _SpecCon, _Sensors, [], Acc) ->
  {lists:reverse(Acc), create_init_pattern(Acc)}.

calculate_optimal_substrate_dimension(Sensors, Actuators) ->
  SFormats = [models:get(format, S) || S <- Sensors],
  AFormats = [models:get(format, A) || A <- Actuators],
  extract_maxdim(SFormats ++ AFormats, []) + 2.

%%-----------------------------------------------------------------------------
%% @doc The generalize_evo_hist generalizes the evolutionary history tuples
%%      by removing the unique element ids. Two neurons which are using
%%      exactly the same activation function, located exactly in the same
%%      layer, and using exactly the same weights will still have different
%%      unique ids, thus these ids must be removed to produce a more general
%%      set of tuples. There are 3 types of tuples in evo_hist list, with 3,
%%      2 and 1 element ids. Once the evolutionary history list is
%%      generalized, it is returned to the caller.
%% @end
%%-----------------------------------------------------------------------------
generalize_evo_hist([{MO, {AType, {ALI, _AUId}}, {BType, {BLI, _BUId}},
  {CType, {CLI, _CUId}}} | EvoHist], Acc) ->
  generalize_evo_hist(EvoHist, [{MO, {ALI, AType}, {BLI, BType}, {CLI, CType}} | Acc]);
generalize_evo_hist([{MO, {AType, {ALI, _AUId}}, {BType, {BLI, _BUId}}} | EvoHist], Acc) ->
  generalize_evo_hist(EvoHist, [{MO, {ALI, AType}, {BLI, BType}} | Acc]);
generalize_evo_hist([{MO, {AType, {ALI, _AUId}}} | EvoHist], Acc) ->
  generalize_evo_hist(EvoHist, [{MO, {ALI, AType}} | Acc]);
generalize_evo_hist([{MO, _EId} | EvoHist], Acc) ->
  generalize_evo_hist(EvoHist, [{MO} | Acc]);
generalize_evo_hist([], Acc) ->
  lists:reverse(Acc).

%%%----------------------------------------------------------------------------
%%% @doc The print/1 accepts an agent's id, and prints out the complete
%%%      genotype of that agent.
%%% @end
%%%----------------------------------------------------------------------------
print(AgentId) ->
  A = db:read(AgentId, agent),
  Cx = db:read(models:get(cx_id, A), cortex),
  ?LOG_INFO("~p~n", [A]),
  ?LOG_INFO("~p~n", [Cx]),
  [?LOG_INFO("~p~n", [db:read(Id, sensor)]) || Id <- models:get(sensor_ids, Cx)],
  [?LOG_INFO("~p~n", [db:read(Id, neuron)]) || Id <- models:get(neuron_ids, Cx)],
  [?LOG_INFO("~p~n", [db:read(Id, actuator)]) || Id <- models:get(actuator_ids, Cx)],
  case models:get(substrate_id, A) of
    not_found ->
      ok;
    SubstrateId ->
      Substrate = db:read(SubstrateId, substrate),
      ?LOG_INFO("~p~n", [Substrate]),
      [?LOG_INFO("~p~n", [db:read(Id, sensor)]) || Id <- models:get(cpp_ids, Substrate)],
      [?LOG_INFO("~p~n", [db:read(Id, actuator)]) || Id <- models:get(cep_ids, Substrate)]
  end.

clone_agent(AgentId, CloneAgentId) ->
  A = db:read(AgentId, agent),
  Cx = db:read(models:get(cx_id, A), cortex),
  IdsNCloneIds = ets:new(idsNcloneids, [set, private,
    {write_concurrency, true}, {read_concurrency, true}]),
  ets:insert(IdsNCloneIds, {bias, bias}),
  ets:insert(IdsNCloneIds, {AgentId, CloneAgentId}),
  [CloneCxId] = map_ids(IdsNCloneIds, [models:get(cx_id, A)], []),
  CloneNIds = map_ids(IdsNCloneIds, models:get(neuron_ids, Cx), []),
  CloneSIds = map_ids(IdsNCloneIds, models:get(sensor_ids, Cx), []),
  CloneAIds = map_ids(IdsNCloneIds, models:get(actuator_ids, Cx), []),
  case models:get(substrate_id, A) of
    undefined ->
      clone_agent_no_substrate(AgentId, CloneAgentId, A, Cx,
        IdsNCloneIds, CloneCxId, CloneNIds, CloneSIds, CloneAIds);
    _SubstrateId ->
      clone_agent_substrate(AgentId, CloneAgentId, A, Cx,
        IdsNCloneIds, CloneCxId, CloneNIds, CloneSIds, CloneAIds)
  end,
  ets:delete(IdsNCloneIds),
  CloneAgentId.

clone_agent_no_substrate(_AgentId, CloneAgentId, A, Cx, IdsNCloneIds,
  CloneCxId, CloneNIds, CloneSIds, CloneAIds) ->
  clone_neurons(IdsNCloneIds, models:get(neuron_ids, Cx)),
  clone_sensors(IdsNCloneIds, models:get(sensor_ids, Cx)),
  clone_actuators(IdsNCloneIds, models:get(actuator_ids, Cx)),
  UEvoHist = map_evo_hist(IdsNCloneIds, models:get(evo_hist, A)),
  db:write(models:set([{id, CloneCxId}, {agent_id, CloneAgentId},
    {sensor_ids, CloneSIds}, {actuator_ids, CloneAIds}, {neuron_ids, CloneNIds}], Cx), cortex),
  db:write(models:set([{id, CloneAgentId}, {cx_id, CloneCxId},
    {offspring_ids, []}, {evo_hist, UEvoHist}], A), agent).

clone_agent_substrate(_AgentId, CloneAgentId, A, Cx, IdsNCloneIds,
  CloneCxId, CloneNIds, CloneSIds, CloneAIds) ->
  Substrate = db:read(models:get(substrate_id, A), substrate),
  [CloneSubstrateId] = map_ids(IdsNCloneIds, [models:get(substrate_id, A)], []),
  CloneCPPIds = map_ids(IdsNCloneIds, models:get(cpp_ids, Substrate), []),
  CloneCEPIds = map_ids(IdsNCloneIds, models:get(cep_ids, Substrate), []),
  clone_neurons(IdsNCloneIds, models:get(neuron_ids, Cx)),
  clone_sensors(IdsNCloneIds, models:get(sensor_ids, Cx)),
  clone_actuators(IdsNCloneIds, models:get(actuator_ids, Cx)),
  Substrate = db:read(models:get(substrate_id, A), substrate),
  clone_sensors(IdsNCloneIds, models:get(cpp_ids, Substrate)),
  clone_actuators(IdsNCloneIds, models:get(cep_ids, Substrate)),
  UEvoHist = map_evo_hist(IdsNCloneIds, models:get(evo_hist, A)),
  db:write(models:set([{id, CloneSubstrateId}, {agent_id, CloneAgentId},
    {cpp_ids, CloneCPPIds}, {cep_ids, CloneCEPIds}], Substrate), substrate),
  db:write(models:set([{id, CloneCxId}, {agent_id, CloneAgentId},
    {sensor_ids, CloneSIds}, {actuator_ids, CloneAIds}, {neuron_ids, CloneNIds}], Cx), cortex),
  db:write(models:set([{id, CloneAgentId}, {cx_id, CloneCxId},
    {substrate_id, CloneSubstrateId}, {offspring_ids, []}, {evo_hist, UEvoHist}], A), agent).

%%-----------------------------------------------------------------------------
%% @doc The generate_neuron_pf accepts a list of plasticity function tags,
%%      and returns a randomly chosen one. If an empty list was passed as the
%%      parameter, the function returns the default none tag.
%% @end
%%-----------------------------------------------------------------------------
generate_neuron_pf(PFNames) ->
  case PFNames of
    [] ->
      {none, []};
    Other ->
      PFName = lists:nth(rand:uniform(length(Other)), Other),
      NLParameters = plasticity:PFName(neural_parameters),
      {PFName, NLParameters}
  end.

%%-----------------------------------------------------------------------------
%% @doc The generate_neuron_aggr_f accepts a list of aggregation function
%%      tags, and returns a randomly chosen one. If an empty list was passed
%%      as the parameter, the function returns the default dot_product tag.
%% @end
%%-----------------------------------------------------------------------------
generate_neuron_aggr_f(AggregationFunctions) ->
  case AggregationFunctions of
    [] ->
      dot_product;
    Other ->
      lists:nth(rand:uniform(length(Other)), Other)
  end.

create_init_pattern([Id | Ids]) ->
  {_, {LI, _}} = Id,
  create_init_pattern(Ids, LI, [Id], []).

create_init_pattern([Id | Ids], CurIndex, CurIndexAcc, PatternAcc) ->
  {_, {LI, _}} = Id,
  case LI == CurIndex of
    true ->
      create_init_pattern(Ids, CurIndex, [Id | CurIndexAcc], PatternAcc);
    false ->
      create_init_pattern(Ids, LI, [Id], [{CurIndex, CurIndexAcc} | PatternAcc])
  end;
create_init_pattern([], CurIndex, CurIndexAcc, PatternAcc) ->
  lists:sort([{CurIndex, CurIndexAcc} | PatternAcc]).

create_input_idps(PF, [{InputId, InputVL} | InputIdPs], Acc) ->
  WeightsP = create_neural_weights_p(PF, InputVL, []),
  create_input_idps(PF, InputIdPs, [{InputId, WeightsP} | Acc]);
create_input_idps(_PF, [], Acc) ->
  Acc.

%%-----------------------------------------------------------------------------
%% @doc The generate_neuron_af accepts a list of activation function tags,
%%      and returns a randomly chosen one. If an empty list was passed as the
%%      parameter, the function returns the default tanh tag.
%% @end
%%-----------------------------------------------------------------------------
generate_neuron_af(ActivationFunctions) ->
  case ActivationFunctions of
    [] ->
      tanh;
    Other ->
      lists:nth(rand:uniform(length(Other)), Other)
  end.

%%-----------------------------------------------------------------------------
%% @doc The function calculate_roids accepts as input the SelfId of the
%%      neuron, and the OutputIds of the elements the neuron connects to.
%%      Since each element specifies its type and, in the case of neurons,
%%      specifies the layer index it belongs to, the function checks if the
%%      OutputId's layer index is lower than the SelfId's layer index, if
%%      it is, the output connection is recursive and the OutputId is added
%%      to the recursive output list. Once the recursive connection ids have
%%      been extracted from the OutputIds, the extracted id list is returned
%%      to the caller.
%% @end
%%-----------------------------------------------------------------------------
calculate_roids(SelfId, [OutputId | Ids], Acc) ->
  case OutputId of
    {_, actuator} ->
      calculate_roids(SelfId, Ids, Acc);
    OutputId ->
      {_NodeType, {TLI, _}} = SelfId,
      {_, {LI, _}} = OutputId,
      case LI =< TLI of
        true ->
          calculate_roids(SelfId, Ids, [OutputId | Acc]);
        false ->
          calculate_roids(SelfId, Ids, Acc)
      end
  end;
calculate_roids(_SelfId, [], Acc) ->
  lists:reverse(Acc).

extract_maxdim([F | Formats], Acc) ->
  DS = case F of
    {symmetric, Dims} ->
      length(Dims);
    no_geo ->
      1;
    undefined ->
      1
  end,
  extract_maxdim(Formats, [DS | Acc]);
extract_maxdim([], Acc) ->
  lists:max(Acc).

get_node_summary(NIds) ->
  get_node_summary(NIds, 0, 0, 0, []).

get_node_summary([NId|NIds], ILAcc, OLAcc, ROAcc, FunctionDistribution) ->
  N = db:read(NId, neuron),
  AF = models:get(af, N),
  ILCount = length(models:get(input_idps, N)),
  OLCount = length(models:get(output_ids, N)),
  ROCount = length(models:get(ro_ids, N)),
  UFunctionDistribution = case lists:keyfind(AF, 1, FunctionDistribution) of
    {AF, Count} ->
      lists:keyreplace(AF, 1, FunctionDistribution, {AF, Count + 1});
    false ->
      [{AF, 1} | FunctionDistribution]
  end,
  get_node_summary(NIds, ILCount + ILAcc, OLCount + OLAcc, ROCount + ROAcc, UFunctionDistribution);
get_node_summary([], ILAcc, OLAcc, ROAcc, FunctionDistribution) ->
  {ILAcc, OLAcc, ROAcc, FunctionDistribution}.

%%-----------------------------------------------------------------------------
%% @doc The map_ids accepts the name of the ets table, and a list of ids.
%%      It then goes through every id and creates a clone version of the id
%%      by generating a new unique id. The function is able to generate new
%%      id structures for neuron, cortex, sensor, and actuator id types.
%% @end
%%-----------------------------------------------------------------------------
map_ids(TableName, [Id | Ids], Acc) ->
  CloneId = case Id of
    {Type, {LayerIndex, _NumId}} ->
      {Type, {LayerIndex, unique_id()}}
  end,
  ets:insert(TableName, {Id, CloneId}),
  map_ids(TableName, Ids, [CloneId | Acc]);
map_ids(_TableName, [], Acc) ->
  Acc.

%%-----------------------------------------------------------------------------
%% @doc The clone_sensors accepts as input the name of the ets table and
%%      the list of sensor ids. It then goes through every sensor id, reads
%%      the sensor from the database, and updates all the ids (id, cx_id,
%%      and fanout_ids) from their original values, to their clone values
%%      stored in the ets table. Then the new version of the sensor is
%%      written to the database.
%% @end
%%-----------------------------------------------------------------------------
clone_sensors(TableName, [SId | SIds]) ->
  S = db:read(SId, sensor),
  CloneSId = ets:lookup_element(TableName, SId, 2),
  CloneCxId = ets:lookup_element(TableName, models:get(cx_id, S), 2),
  CloneFanoutIds = [ets:lookup_element(TableName, FanoutId, 2) ||
  FanoutId <- models:get(fanout_ids, S)],
  db:write(models:set([{id, CloneSId}, {cx_id, CloneCxId},
    {fanout_ids, CloneFanoutIds}], S), sensor),
  clone_sensors(TableName, SIds);
clone_sensors(_TableName, []) ->
  done.

%%-----------------------------------------------------------------------------
%% @doc The clone_actuators accepts as input the name of the ets table and
%%      the list of actuator ids. It then goes through every actuator id,
%%      reads the actuator from the database, and updates all the ids (id,
%%      cx_id, and fanin_ids) from their original values, to their clone
%%      values stored in the ets table. Then the new version of the actuator
%%      is written to the database.
%% @end
%%-----------------------------------------------------------------------------
clone_actuators(TableName, [AId | AIds]) ->
  A = db:read(AId, actuator),
  CloneAId = ets:lookup_element(TableName, AId, 2),
  CloneCxId = ets:lookup_element(TableName, models:get(cx_id, A), 2),
  CloneFaninIds = [ets:lookup_element(TableName, FaninId, 2)||FaninId <- models:get(fanin_ids, A)],
  db:write(models:set([{id, CloneAId}, {cx_id, CloneCxId},
    {fanin_ids, CloneFaninIds}], A), actuator),
  clone_actuators(TableName, AIds);
clone_actuators(_TableName, []) ->
  done.

%%-----------------------------------------------------------------------------
%% @doc The clone_neuron accepts as input the name of the ETS table and the
%%      list of neuron ids. It then goes through every neuron id, reads the
%%      neuron from the database, and updates all the ids (id, cx_id,
%%      output_ids, ro_ids) and input_idps from their original values, to
%%      their clone values stored in the ETS table. Once the everything is
%%      updated, the new (clone) version of the neuron is written to the
%%      database.
%% @end
%%-----------------------------------------------------------------------------
clone_neurons(TableName, [NId | NIds]) ->
  N = db:read(NId, neuron),
  CloneNId = ets:lookup_element(TableName, NId, 2),
  CloneCxId = ets:lookup_element(TableName, models:get(cx_id, N), 2),
  CloneInputIdPs =  [{ets:lookup_element(TableName, IId, 2), WeightsP} || {IId, WeightsP}
  <- models:get(input_idps, N)],
  CloneInputIdPsModulation = [{ets:lookup_element(TableName, IId, 2), WeightsP} || {IId, WeightsP}
  <- models:get(input_idps_modulation, N)],
  CloneOutputIds = [ets:lookup_element(TableName, OId, 2) || OId <- models:get(output_ids, N)],
  CloneROIds = [ets:lookup_element(TableName, ROId, 2) || ROId <- models:get(ro_ids, N)],
  db:write(models:set([{id, CloneNId}, {cx_id, CloneCxId},
    {input_idps, CloneInputIdPs}, {input_idps_modulation, CloneInputIdPsModulation},
    {output_ids, CloneOutputIds}, {ro_ids, CloneROIds}], N), neuron),
  clone_neurons(TableName, NIds);
clone_neurons(_TableName, []) ->
  done.

%%-----------------------------------------------------------------------------
%% @doc The map_evo_hist is a wrapper for map_evo_hist/3, which in turn
%%      accepts the evo_hist list containing the mutation operator tuples
%%      that have been applied to the NN system. The function is used when
%%      a clone of a NN system is created. The function updates the original
%%      Ids of the elements the mutation operators have been applied to, to
%%      the clone's Ids, so that the updated evo_hist can reflect the clone's
%%      topology, as if the mutation operators have been applied to it, and
%%      that it is not a clone. Once all the tuples in the evo_hist have been
%%      updated with the clone element ids, the list is reverted to its
%%      proper order, and the updated list is returned to the caller.
%% @end
%%-----------------------------------------------------------------------------
map_evo_hist(TableName, EvoHist) ->
  map_evo_hist(TableName, EvoHist, []).

map_evo_hist(TableName, [{MO, E1Id, E2Id, E3Id} | EvoHist], Acc) ->
  CloneE1Id = ets:lookup_element(TableName, E1Id, 2),
  CloneE2Id = ets:lookup_element(TableName, E2Id, 2),
  CloneE3Id = ets:lookup_element(TableName, E3Id, 2),
  map_evo_hist(TableName, EvoHist, [{MO, CloneE1Id, CloneE2Id, CloneE3Id} | Acc]);
map_evo_hist(TableName, [{MO, E1Id, E2Id} | EvoHist], Acc) ->
  CloneE1Id = ets:lookup_element(TableName, E1Id, 2),
  CloneE2Id = ets:lookup_element(TableName, E2Id, 2),
  map_evo_hist(TableName, EvoHist, [{MO, CloneE1Id, CloneE2Id} | Acc]);
map_evo_hist(TableName, [{MO, E1Ids} | EvoHist], Acc) when is_list(E1Ids) ->
  CloneE1Ids = [ets:lookup_element(TableName, E1Id, 2) || E1Id <- E1Ids],
  map_evo_hist(TableName, EvoHist, [{MO, CloneE1Ids} | Acc]);
map_evo_hist(TableName, [{MO, E1Id} | EvoHist], Acc) ->
  CloneE1Id = ets:lookup_element(TableName, E1Id, 2),
  map_evo_hist(TableName, EvoHist, [{MO, CloneE1Id} | Acc]);
map_evo_hist(_TableName, [], Acc) ->
  lists:reverse(Acc);
map_evo_hist(TableName, Uknown, Acc) ->
  logr:error({genotype, map_evo_hist, error, "can't find the proper pattern match",
    [TableName, Uknown, Acc]}),
  exit("genotype:map_evo_hist - can't find the proper pattern match").