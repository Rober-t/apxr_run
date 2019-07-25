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
%%% @doc The genome_mutator module is a container for the
%%%      genome_mutator functions. We need to have a set of mutation operators
%%%      which are flexible enough so that any NN topology A can be turned
%%%      into a NN topology B, by applying the available mutation operators to
%%%      the NN system in some sequence. It is only then that our
%%%      neuroevolutionary system will have the necessary tools and flexibility
%%%      to evolve any type of NN based system given enough time and the
%%%      appropriate fitness function.
%%%      Technically, we do not need every one of these mutation operators; the
%%%      following list will be enough for a highly versatile complexifying
%%%      topology and weight evolving artificial neural network (TWEANN) system:
%%%      mutate_weights, add_bias, remove_bias, mutate_af, add_neuron, splice
%%%      (just one of them), add_inlink, add_outlink, add_sensorlink,
%%%      add_actuatorlink, add_sensor, and add_actuator. Note that this
%%%      combination of MOs can convert any NN topology A into a NN topology B,
%%%      given that A is contained (smaller, and simpler in a sense) within B.
%%%      The add_inlink, add_outlink, add_sensorlink, add_actuatorlink mutation
%%%      operators allow for neurons to form new connections to neurons,
%%%      sensors and actuators. The add_sensor and add_actuator, can
%%%      add/integrate the new sensor and actuator programs into the NN system.
%%%      The add_neuron will add new neurons in parallel with other neurons in
%%%      a layer, while outsplice will create new layers, increasing the
%%%      depth of the NN system, and form new connections in series. The weight
%%%      perturbations will be performed by the exoself, in a separate phase
%%%      from the topological mutation phase, which will effectively make our
%%%      system a memetic algorithm based TWEANN. On the other hand, if we also
%%%      add mutate_weights operator to the mutation phase, and remove the
%%%      exoself's weight tunning/perturbing ability in its separate phase, then
%%%      then our system will become a standard genetic algorithm based TWEANN.
%%% @end
%%%----------------------------------------------------------------------------
-module(genome_mutator).

%% API
-export([
  mutate/1,
  mutate_tuning_selection/1,
  mutate_tuning_annealing/1,
  mutate_tot_topological_mutations/1,
  mutate_heredity_type/1,
  mutate_weights/1,
  add_bias/1,
  remove_bias/1,
  mutate_af/1,
  mutate_pf/1,
  mutate_plasticity_parameters/1,
  mutate_aggr_f/1,
  link_from_element_to_element/3,
  link_from_neuron_to_neuron/3,
  link_from_sensor_to_neuron/3,
  link_from_neuron_to_actuator/3,
  cutlink_from_element_to_element/3,
  cutlink_from_neuron_to_neuron/3,
  cutlink_from_sensor_to_neuron/3,
  cutlink_from_neuron_to_actuator/3,
  add_outlink/1,
  add_inlink/1,
  add_neuron/1,
  outsplice/1,
  get_new_li/4,
  add_sensorlink/1,
  add_actuatorlink/1,
  add_sensor/1,
  add_actuator/1,
  add_cpp/1,
  add_cep/1
]).

%% Xref
-ignore_xref([
  mutate/1,
  mutate_tuning_selection/1,
  mutate_tuning_annealing/1,
  mutate_tot_topological_mutations/1,
  mutate_heredity_type/1,
  mutate_weights/1,
  add_bias/1,
  remove_bias/1,
  mutate_af/1,
  mutate_pf/1,
  mutate_plasticity_parameters/1,
  mutate_aggr_f/1,
  link_from_element_to_element/3,
  link_from_neuron_to_neuron/3,
  link_from_sensor_to_neuron/3,
  link_from_neuron_to_actuator/3,
  cutlink_from_element_to_element/3,
  cutlink_from_neuron_to_neuron/3,
  cutlink_from_sensor_to_neuron/3,
  cutlink_from_neuron_to_actuator/3,
  add_outlink/1,
  add_inlink/1,
  add_neuron/1,
  outsplice/1,
  get_new_li/4,
  add_sensorlink/1,
  add_actuatorlink/1,
  add_sensor/1,
  add_actuator/1,
  add_cpp/1,
  add_cep/1
]).

%%%============================================================================
%%% Configuration
%%%============================================================================

-define(SAT_LIMIT, math:pi() * 2).
-define(DELTA_MULTIPLIER, math:pi() * 2).
-define(ESMUTATORS, [
  mutate_tuning_selection,
  mutate_tuning_annealing,
  mutate_tot_topological_mutations,
  mutate_heredity_type
]).

%%%============================================================================
%%% API
%%%============================================================================

%%-----------------------------------------------------------------------------
%% @doc The function mutate first updates the generation of the agent to be
%%      mutated, then calculates the number of mutation operators to be
%%      applied to it by executing the tot_topological_mutations:TTMName/2
%%      function, and then finally runs the apply_mutators/2 function, which
%%      mutates the agent. Once the agent is mutated, the function updates
%%      its fingerprint by executing genotype:update_finrgerprint/1.
%% @end
%%-----------------------------------------------------------------------------
-spec mutate(models:agent_id()) -> ok.
mutate(AgentId) ->
  utils:random_seed(),
  mutate_search_parameters(AgentId),
  A = db:read(AgentId, agent),
  {TTMName, Parameter} = models:get(tot_topological_mutations_f, A),
  TotMutations = tot_topological_mutations:TTMName(Parameter, AgentId),
  OldGeneration = models:get(generation, A),
  NewGeneration = OldGeneration + 1,
  db:write(models:set({generation, NewGeneration}, A), agent),
  apply_mutators(AgentId, TotMutations),
  genotype:update_fingerprint(AgentId).

%%-----------------------------------------------------------------------------
%% @doc The mutate_tuning_selection function checks if there are any other
%%      than the currently used tuning selection functions available in the
%%      agent's constraint. If there is, then it chooses a random one from
%%      this list, and sets the agent's tuning_selection_f to it. If there
%%      are no other tuning selection functions, then it exits with an error.
%% @end
%%-----------------------------------------------------------------------------
-spec mutate_tuning_selection(models:agent_id()) -> ok.
mutate_tuning_selection(AgentId) ->
  A = db:read(AgentId, agent),
  case models:get(tuning_selection_fs, models:get(constraint, A)) --
    [models:get(tuning_selection_f, A)] of
      [] ->
        false;
      TuningSelectionFunctions ->
        UA = models:set({tuning_selection_f, new_tsf(TuningSelectionFunctions)}, A),
        db:write(UA, agent)
  end.

%%-----------------------------------------------------------------------------
%% @doc The mutate_annealing_parameter function checks if there are any
%%      other than the currently used tuning annealing parameters available
%%      in the agent's constraint. If there is, then it chooses a random one
%%      from this list, and sets the agent's annealing_parameter to it. If
%%      there are no other tuning annealing parameters, then it exits with
%%      an error.
%% @end
%%-----------------------------------------------------------------------------
-spec mutate_tuning_annealing(models:agent_id()) -> ok.
mutate_tuning_annealing(AgentId) ->
  A = db:read(AgentId, agent),
  case models:get(annealing_parameters, models:get(constraint, A)) --
    [models:get(annealing_parameter, A)] of
      [] ->
        false;
      TuningAnnealingParams ->
        NewTAP = lists:nth(rand:uniform(length(TuningAnnealingParams)), TuningAnnealingParams),
        UA = models:set({annealing_parameter, NewTAP}, A),
        db:write(UA, agent)
  end.

%%-----------------------------------------------------------------------------
%% @doc The mutate_tot_topological_mutations function checks if there are
%%      any other than the currently used tuning tot topological mutation
%%      functions available in the agent's constraint. If there is, then it
%%      chooses a random one from this list, and sets the agent's
%%      tot_topological_mutations_f to it. If there are no other functions
%%      that can calculate tot topological mutations, then it exits with an
%%      error.
%% @end
%%-----------------------------------------------------------------------------
-spec mutate_tot_topological_mutations(models:agent_id()) -> ok.
mutate_tot_topological_mutations(AgentId) ->
  A = db:read(AgentId, agent),
  case models:get(tot_topological_mutations_fs, models:get(constraint, A)) --
    [models:get(tot_topological_mutations_f, A)] of
      [] ->
        false;
      TotTopologicalMutations ->
        NewTTF = lists:nth(rand:uniform(length(TotTopologicalMutations)), TotTopologicalMutations),
        UA = models:set({tot_topological_mutations_f, NewTTF}, A),
        db:write(UA, agent)
  end.

%%-----------------------------------------------------------------------------
%% @doc The mutate_heredity_type function checks if there are any other
%%      heredity types in the agent's constraint record. If any other than
%%      the one currently used by the agent are present, the agent exchanges
%%      the heredity type it currently uses to a random one from the remaining
%%      list. If no other heredity types are available, the mutation operator
%%      exits with an error, and the neuroevolutionary system tries another
%%      mutation operator.
%% @end
%%-----------------------------------------------------------------------------
-spec mutate_heredity_type(models:agent_id()) -> ok.
mutate_heredity_type(AgentId) ->
  A = db:read(AgentId, agent),
  case models:get(heredity_types, models:get(constraint, A)) -- [models:get(heredity_type, A)] of
    [] ->
      false;
    HeredityTypePool->
      NewHT = lists:nth(rand:uniform(length(HeredityTypePool)), HeredityTypePool),
      UA = models:set({heredity_type, NewHT}, A),
      db:write(UA, agent)
  end.

%%-----------------------------------------------------------------------------
%% @doc The mutate_weights function accepts the AgentId parameter, extracts
%%      the NN's cortex, and then chooses a random neuron belonging to the NN
%%      with a uniform distribution probability. Then the neuron's input_idps
%%      list is extracted, and the function perturb_idps/1 is used to
%%      perturb/mutate the weights. Once the InputIdPs have been perturbed,
%%      the agent's evolutionary history, EvoHist is updated to include the
%%      successfully applied mutate_weights mutation operator. Then the
%%      updated Agent and the updated neuron are written to the database.
%% @end
%%-----------------------------------------------------------------------------
-spec mutate_weights(models:agent_id()) -> ok.
mutate_weights(AgentId) ->
  A = db:read(AgentId, agent),
  CxId = models:get(cx_id, A),
  Cx = db:read(CxId, cortex),
  NIds = models:get(neuron_ids, Cx),
  Generation = models:get(generation, A),
  [PerturbationRange, PerturbationQty, AnnealingParam, TuningSelectionFunc] = models:get(
    [perturbation_range, perturbation_qty, annealing_parameter, tuning_selection_f], A),
  PerturbedNIds = case PerturbationQty of
    multiple ->
      %%% Multiple Neurons Perturbed
        ChosenNIdPs = tuning_selection:TuningSelectionFunc(NIds, Generation, PerturbationRange,
          AnnealingParam),
        [mutate_weights(NId, Spread) || {NId, Spread} <- ChosenNIdPs], [NId || {NId, _Spread} <-
        ChosenNIdPs];
      %%%
    single ->
      %%% One Neuron Perturbed
        NId = lists:nth(rand:uniform(length(NIds)), NIds),
        N = db:read(NId, neuron),
        InputIdPs = models:get(input_idps, N),
        UInputIdPs = perturb_idps(InputIdPs),
        db:write(models:set({input_idps, UInputIdPs}, N), neuron),
        NId
      %%%
  end,
  EvoHist = models:get(evo_hist, A),
  UEvoHist = [{mutate_weights, PerturbedNIds} | EvoHist],
  UA = models:set({evo_hist, UEvoHist}, A),
  db:write(UA, agent).

%%-----------------------------------------------------------------------------
%% @doc The add_bias function is called with the AgentId parameter. The
%%      function first extracts the neuron_ids list from the cortex element
%%      and chooses a random neuron from the id list. After the neuron is
%%      read from the database, we check whether input_idps and
%%      input_idps_modulation lists already have bias, and we randomly
%%      generate a value 1 or 2. If the value 1 is generated and the
%%      input_idps list does not have a bias, it is added. If the value 2 is
%%      generated, and the input_idps_modulation does not have a bias, it is
%%      added. Otherwise an error is returned.
%% @end
%%-----------------------------------------------------------------------------
-spec add_bias(models:agent_id()) -> ok.
add_bias(AgentId) ->
  A = db:read(AgentId, agent),
  CxId = models:get(cx_id, A),
  Cx = db:read(CxId, cortex),
  NIds = models:get(neuron_ids, Cx),
  NId = lists:nth(rand:uniform(length(NIds)), NIds),
  Generation = models:get(generation, A),
  N = db:read(NId, neuron),
  [SIIdPs, MIIdPs] = models:get([input_idps, input_idps_modulation], N),
  {PFName, _NLParameters} = models:get(pf, N),
  case check_bias(SIIdPs, MIIdPs, PFName) of
    {_, false, true, 2} ->
      UMIIdPs = do_add_bias(MIIdPs, PFName),
      UN = models:set([{input_idps_modulation, UMIIdPs}, {generation, Generation}], N),
      EvoHist = models:get(evo_hist, A),
      UEvoHist = [{{add_bias, m}, NId} | EvoHist],
      UA = models:set({evo_hist, UEvoHist}, A),
      db:write(UN, neuron),
      db:write(UA, agent);
    {true, _, _, _} ->
      false; % Neuron already has a bias in input_idps
    {false, _, _, _} ->
      USIIdPs = do_add_bias(SIIdPs, PFName),
      UN = models:set([{input_idps, USIIdPs}, {generation, Generation}], N),
      EvoHist = models:get(evo_hist, A),
      UEvoHist = [{{add_bias, s}, NId} | EvoHist],
      UA = models:set({evo_hist, UEvoHist}, A),
      db:write(UN, neuron),
      db:write(UA, agent)
  end.

%%-----------------------------------------------------------------------------
%% @doc The remove_bias function is called with the AgentId parameter. The
%%      function first extracts the neuron_ids list from the cortex element
%%      and chooses a random neuron from the id list. After the neuron is
%%      read from the database, we check whether input_idps and
%%      input_idps_modulation lists already have bias, and we randomly
%%      generate a value 1 or 2. If the value 1 is generated and the
%%      input_idps list has a bias, it is removed. If the value 2 is
%%      generated, and the input_idps_modulation has a bias, it is removed.
%%      Otherwise an error is returned.
%% @end
%%-----------------------------------------------------------------------------
-spec remove_bias(models:agent_id()) -> ok.
remove_bias(AgentId) ->
  A = db:read(AgentId, agent),
  CxId = models:get(cx_id, A),
  Cx = db:read(CxId, cortex),
  NIds = models:get(neuron_ids, Cx),
  NId = lists:nth(rand:uniform(length(NIds)), NIds),
  Generation = models:get(generation, A),
  N = db:read(NId, neuron),
  [SIIdPs, MIIdPs] = models:get([input_idps, input_idps_modulation], N),
  {PFName, _NLParameters} = models:get(pf, N),
  case check_bias(SIIdPs, MIIdPs, PFName) of
    {_, true, true, 2} -> % Remove modulatory bias
      UMIIdPs = lists:keydelete(bias, 1, MIIdPs),
      UN = models:set([{input_idps_modulation, UMIIdPs}, {generation, Generation}], N),
      EvoHist = models:get(evo_hist, A),
      UEvoHist = [{{remove_bias, m}, NId} | EvoHist],
      UA = models:set({evo_hist, UEvoHist}, A),
      db:write(UN, neuron),
      db:write(UA, agent);
    {false, _, _, _} ->
      false; % Neuron does not have a bias in input_idps
    {true, _, _, _} -> % Remove synaptic bias
      USIIdPs = lists:keydelete(bias, 1, SIIdPs),
      UN = models:set([{input_idps, USIIdPs}, {generation, Generation}], N),
      EvoHist = models:get(evo_hist, A),
      UEvoHist = [{{remove_bias, s}, NId} | EvoHist],
      UA = models:set({evo_hist, UEvoHist}, A),
      db:write(UN, neuron),
      db:write(UA, agent)
  end.

%%-----------------------------------------------------------------------------
%% @doc The mutate_af function chooses a random neuron, and then changes its
%%      currently used activation function into another one available from the
%%      neural_afs list of the agent's constraint record.
%% @end
%%-----------------------------------------------------------------------------
-spec mutate_af(models:agent_id()) -> ok.
mutate_af(AgentId) ->
  A = db:read(AgentId, agent),
  CxId = models:get(cx_id, A),
  Cx = db:read(CxId, cortex),
  NIds = models:get(neuron_ids, Cx),
  NId = lists:nth(rand:uniform(length(NIds)), NIds),
  Generation = models:get(generation, A),
  N = db:read(NId, neuron),
  AF = models:get(af, N),
  case models:get(neural_afs, models:get(constraint, A)) -- [AF] of
    [] ->
      false;
    ActivationFunctions ->
      NewAF = lists:nth(rand:uniform(length(ActivationFunctions)), ActivationFunctions),
      UN = models:set([{af, NewAF}, {generation, Generation}], N),
      EvoHist = models:get(evo_hist, A),
      UEvoHist = [{mutate_af, NId} | EvoHist],
      UA = models:set({evo_hist, UEvoHist}, A),
      db:write(UN, neuron),
      db:write(UA, agent)
  end.

%%-----------------------------------------------------------------------------
%% @doc The mutate_pf function chooses a random neuron, and then changes
%%      its currently used plasticity function into another one available
%%      from the neural_pfs list of the agent's constraint record.
%% @end
%%-----------------------------------------------------------------------------
-spec mutate_pf(models:agent_id()) -> ok.
mutate_pf(AgentId) ->
  A = db:read(AgentId, agent),
  CxId = models:get(cx_id, A),
  Cx = db:read(CxId, cortex),
  NIds = models:get(neuron_ids, Cx),
  NId = lists:nth(rand:uniform(length(NIds)), NIds),
  Generation = models:get(generation, A),
  N = db:read(NId, neuron),
  {PFName, _NLParameters} = models:get(pf, N),
  case models:get(neural_pfns, models:get(constraint, A)) -- [PFName] of
    [] ->
      false;
    OtherPFNames ->
      NewPFName = lists:nth(rand:uniform(length(OtherPFNames)), OtherPFNames),
      NewNLParameters = plasticity:NewPFName(neural_parameters),
      NewPF = {NewPFName, NewNLParameters},
      InputIdPs = models:get(input_idps, N),
      UInputIdPs = [{InputIdP, plasticity:NewPFName(weight_parameters)} || {InputIdP, _OldPL} <-
      InputIdPs],
      UN = models:set([{pf, NewPF}, {input_idps, UInputIdPs}, {generation, Generation}], N),
      EvoHist = models:get(evo_hist, A),
      UEvoHist = [{mutate_pf, NId} | EvoHist],
      UA = models:set({evo_hist, UEvoHist}, A),
      db:write(UN, neuron),
      db:write(UA, agent)
  end.

%%-----------------------------------------------------------------------------
%% @doc The mutate_plasticity_parameters chooses a random neuron from the
%%      NN, and mutates the parameters of its plasticity function, if present.
%% @end
%%-----------------------------------------------------------------------------
-spec mutate_plasticity_parameters(models:agent_id()) -> ok.
mutate_plasticity_parameters(AgentId) ->
  A = db:read(AgentId, agent),
  CxId = models:get(cx_id, A),
  Cx = db:read(CxId, cortex),
  NIds = models:get(neuron_ids, Cx),
  NId = lists:nth(rand:uniform(length(NIds)), NIds),
  N = db:read(NId, neuron),
  {PFName, _Parameters} = models:get(pf, N),
  UN = plasticity:PFName({NId, mutate}),
  EvoHist = models:get(evo_hist, A),
  UEvoHist = [{mutate_plasticity_parameters, NId} | EvoHist],
  UA = models:set({evo_hist, UEvoHist}, A),
  db:write(UN, neuron),
  db:write(UA, agent).

%%-----------------------------------------------------------------------------
%% @doc The mutate_aggr_f function chooses a random neuron, and then changes
%%      its currently used aggregation function into another one available
%%      from the neural_aggr_fs list of the agent's constraint record.
%% @end
%%-----------------------------------------------------------------------------
-spec mutate_aggr_f(models:agent_id()) -> ok.
mutate_aggr_f(AgentId) ->
  A = db:read(AgentId, agent),
  CxId = models:get(cx_id, A),
  Cx = db:read(CxId, cortex),
  NIds = models:get(neuron_ids, Cx),
  NId = lists:nth(rand:uniform(length(NIds)), NIds),
  Generation = models:get(generation, A),
  N = db:read(NId, neuron),
  AggrF = models:get(aggr_f, N),
  case models:get(neural_aggr_fs, models:get(constraint, A)) -- [AggrF] of
    [] ->
      false;
    AggregationFunctions ->
      NewAggrF = lists:nth(rand:uniform(length(AggregationFunctions)), AggregationFunctions),
      UN = models:set([{aggr_f, NewAggrF}, {generation, Generation}], N),
      EvoHist = models:get(evo_hist, A),
      UEvoHist = [{mutate_aggr_f, NId} | EvoHist],
      UA = models:set({evo_hist, UEvoHist}, A),
      db:write(UN, neuron),
      db:write(UA, agent)
  end.

%%-----------------------------------------------------------------------------
%% @doc The function link_from_element_to_element first calculates what type
%%      of link is going to be established (neuron to neuron, sensor to neuron,
%%      or neuron to actuator), and then calls the specific linking function
%%      based on that.
%% @end
%%-----------------------------------------------------------------------------
-spec link_from_element_to_element(non_neg_integer(), models:neuron_id() | models:sensor_id(),
  models:actuator_id() | models:neuron_id()) -> ok.
link_from_element_to_element(Generation, FromElementId, ToElementId) ->
  case {FromElementId, ToElementId} of
    {{neuron, _FromSId}, {neuron, _ToSId}} ->
      link_from_neuron_to_neuron(Generation, FromElementId, ToElementId);
    {{sensor, _FromSId}, {neuron, _ToSId}} ->
      link_from_sensor_to_neuron(Generation, FromElementId, ToElementId);
    {{neuron, _FromNId}, {actuator, _ToAId}} ->
      link_from_neuron_to_actuator(Generation, FromElementId, ToElementId)
  end.

%%-----------------------------------------------------------------------------
%% @doc The link_from_neuron_to_neuron establishes a link from neuron with
%%      id FromNeuronId, to a neuron with id ToNeuronId. The function then
%%      calls link_from_neuron, which establishes the link on the
%%      FromNeuronId's side. The updated neuron associated with the
%%      FromNeuronId is then written to database. To decide how long the
%%      weight list that is going to be added to the ToNeuronId's input_idps,
%%      the function calculates FromNeuronId's output vector length.
%%      Since the connection is from a neuron, FromOVL is set to 1.
%%      link_to_neuron is then called, and the link is established on the
%%      ToNeuronId's side. Finally, the updated neuron associated with the id
%%      ToNeuronId is written to database. The order of reading the FromN and
%%      ToN neuron records from the database is important. It is essential
%%      that ToN is read after the UFromN is written to database, in the case
%%      that FromNeuronId and ToNeuronId refer to the same neuron (a
%%      recurrent connection from the neuron to itself). If both neurons are
%%      read at the same time for example before the links are established,
%%      then the link established in the UFromN will be overwritten when the
%%      UToN is written to file. Thus order is important in this function.
%% @end
%%-----------------------------------------------------------------------------
-spec link_from_neuron_to_neuron(non_neg_integer(), models:neuron_id(), models:actuator_id() |
  models:neuron_id()) -> ok.
link_from_neuron_to_neuron(Generation, FromNeuronId, ToNeuronId) ->
  % A = db:read(AgentId, agent),
  % Generation = models:get(generation, A),
  % From Part
  FromN = db:read(FromNeuronId, neuron),
  UFromN = link_from_neuron(FromN, ToNeuronId, Generation),
  db:write(UFromN, neuron),
  % To Part
  ToN = db:read(ToNeuronId, neuron), % We read it afterwards, in the
  % case that it's the same Element. Thus we do not overwrite the earlier
  % changes.
  FromOVL = 1,
  UToN = link_to_neuron(FromNeuronId, FromOVL, ToN, Generation),
  db:write(UToN, neuron).

%%-----------------------------------------------------------------------------
%% @doc The function link_from_sensor_to_neuron establishes a connection from
%%      the sensor with id FromSensorId, and the neuron with id ToNeuronId.
%%      First the sensor record is updated with the connection details using
%%      the function link_from_sensor, and the updated sensor record is
%%      written to database. Then the record of the neuron to whom the link
%%      is being established is updated using the function link_to_neuron/4,
%%      after which the updated neuron is written to database.
%% @end
%%-----------------------------------------------------------------------------
-spec link_from_sensor_to_neuron(non_neg_integer(), models:sensor_id(),
  {neuron, {void | float(), float()}}) -> ok.
link_from_sensor_to_neuron(Generation, FromSensorId, ToNeuronId) ->
  % A = db:read(AgentId, agent),
  % Generation = models:get(generation, A),
  % From Part
  FromS = db:read(FromSensorId, sensor),
  UFromS = link_from_sensor(FromS, ToNeuronId, Generation),
  db:write(UFromS, sensor),
  % To Part
  ToN = db:read(ToNeuronId, neuron),
  FromOVL = models:get(vl, FromS),
  UToN = link_to_neuron(FromSensorId, FromOVL, ToN, Generation),
  db:write(UToN, neuron).

%%-----------------------------------------------------------------------------
%% @doc The function Link_FromNeuronToActuator establishes a link emanating
%%      from the neuron with an id FromNeuronId, to an actuator with the id
%%      ToActuatorId. First the FromNeuronId's record is updated using the
%%      function link_from_neuron/3, after which the updated neuron record is
%%      written to database. Then the function checks whether the actuator to
%%      which the neuron is establishing the link, still has space for that
%%      link (length(FaninIds) is less than the actuator's vector length,
%%      vl). If there is no more room, then the function exits with error,
%%      if there is room, then the actuator's fanin_ids list is updated by
%%      appending to it the id of the neuron's id. The updated actuator is
%%      then written to the database.
%% @end
%%-----------------------------------------------------------------------------
-spec link_from_neuron_to_actuator(non_neg_integer(), {neuron, {void | float(), float()}},
  {actuator | neuron, {float(), float()}}) -> ok.
link_from_neuron_to_actuator(Generation, FromNeuronId, ToActuatorId) ->
  % A = db:read(AgentId, agent),
  % Generation = models:get(generation, A),
  % From Part
  FromN = db:read(FromNeuronId, neuron),
  UFromN = link_from_neuron(FromN, ToActuatorId, Generation),
  %To Part
  ToA = db:read(ToActuatorId, actuator),
  FaninIds = models:get(fanin_ids, ToA),
  case get_ivl(FaninIds, 0) >= models:get(vl, ToA) of
    true ->
      false; % Fully connected
    false ->
      UFaninIds = [FromNeuronId | FaninIds],
      db:write(UFromN, neuron),
      db:write(models:set([{fanin_ids, UFaninIds}, {generation, Generation}], ToA), actuator)
  end.

%%-----------------------------------------------------------------------------
%% @doc The cutlink_from_element_to_element first checks which of the three
%%      types of connections is between the FromElementId and ToElementId
%%      (neuron to neuron, sensor to neuron, or neuron to actuator), and then
%%      disconnects the two elements using one of the three specialized
%%      cutlink_... functions.
%% @end
%%-----------------------------------------------------------------------------
-spec cutlink_from_element_to_element(non_neg_integer(), models:sensor_id() | models:neuron_id(),
  models:actuator_id() | models:neuron_id()) -> ok.
cutlink_from_element_to_element(Generation, FromElementId, ToElementId) ->
  case {FromElementId, ToElementId} of
    {{neuron, _FromId}, {neuron, _ToId}} ->
      cutlink_from_neuron_to_neuron(Generation, FromElementId, ToElementId);
    {{sensor, _FromId}, {neuron, _ToId}} ->
      cutlink_from_sensor_to_neuron(Generation, FromElementId, ToElementId);
    {{neuron, _FromId}, {actuator, _ToId}} ->
      cutlink_from_neuron_to_actuator(Generation, FromElementId, ToElementId)
  end.

%%-----------------------------------------------------------------------------
%% @doc The cutlink_from_neuron_to_neuron function disconnects the connection
%%      from the FromNeuronId to the ToNeuronId. The function first
%%      disconnects the neuron associated with FromNeuronId by calling the
%%      cutlink_from_neuron, and then writing to database the updated neuron.
%%      The function then disconnects the neuron associated with the
%%      ToNeuronId from the connection using the cutlink_to_neuron/3, and
%%      writes to database the updated ToN record. If the FromNeuronId and
%%      the ToNeuronId are ids of the same neuron, then it is important to
%%      first write UFromN to database, before reading the ToN neuron from
%%      the database, so as not to lose the update made by the
%%      cutlink_from_neuron/3, before reading the updated neuron from the
%%      database and calling the cutlink_to_neuron. Thus this order of
%%      reading and writing the neurons from the database is essential to
%%      cover the corner cases.
%% @end
%%-----------------------------------------------------------------------------
-spec cutlink_from_neuron_to_neuron(non_neg_integer(), models:sensor_id() | models:neuron_id(),
  models:actuator_id() | models:neuron_id()) -> ok.
cutlink_from_neuron_to_neuron(Generation, FromNeuronId, ToNeuronId) ->
  % A = db:read(AgentId, agent),
  % Generation = models:get(generation, A),
  % From Part
  FromN = db:read(FromNeuronId, neuron),
  UFromN = cutlink_from_neuron(FromN, ToNeuronId, Generation),
  db:write(UFromN, neuron),
  % To Part
  ToN = db:read(ToNeuronId, neuron),
  UToN = cutlink_to_neuron(FromNeuronId, ToN, Generation),
  db:write(UToN, neuron).

%%-----------------------------------------------------------------------------
%% @doc The cutlink_from_sensor_to_neuron cuts the connection from the
%%      FromSensorId to ToNeuronId. The function first cuts the connection
%%      on the FromSensorId side using the cutlink_from_sensor/3 side, and
%%      writes the updated sensor to database. The function then cuts the
%%      connection on the ToNeuronId side using the cutlink_to_neuron/3
%%      function, and writes the updated neuron record to database.
%% @end
%%-----------------------------------------------------------------------------
-spec cutlink_from_sensor_to_neuron(non_neg_integer(), models:sensor_id(), models:neuron_id())-> ok.
cutlink_from_sensor_to_neuron(Generation, FromSensorId, ToNeuronId) ->
  % A = db:read(AgentId, agent),
  % Generation = models:get(generation, A),
  % From Part
  FromS = db:read(FromSensorId, sensor),
  UFromS = cutlink_from_sensor(FromS, ToNeuronId, Generation),
  db:write(UFromS, sensor),
  % To Part
  ToN = db:read(ToNeuronId, neuron),
  UToN = cutlink_to_neuron(FromSensorId, ToN, Generation),
  db:write(UToN, neuron).

%%-----------------------------------------------------------------------------
%% @doc cutlink_from_neuron_to_actuator cuts the connection from the
%%      FromNeuronId to ToActuatorId. The function first cuts the connection
%%      on the FromNeuronId side using the cutlink_from_neuron/3 function, and
%%      writes the updated UFromN to database. Then the connection on the
%%      ToActuatorId is cut using the cutlink_to_actuator/3 function, after
%%      which the updated actuator record is written to the database.
%% @end
%%-----------------------------------------------------------------------------
-spec cutlink_from_neuron_to_actuator(non_neg_integer(), models:neuron_id(),
  models:actuator_id()) -> ok.
cutlink_from_neuron_to_actuator(Generation, FromNeuronId, ToActuatorId) ->
  % A = db:read(AgentId, agent),
  % Generation = models:get(generation, A),
  % From Part
  FromN = db:read(FromNeuronId, neuron),
  UFromN = cutlink_from_neuron(FromN, ToActuatorId, Generation),
  db:write(UFromN, neuron),
  % To Part
  ToA = db:read(ToActuatorId, actuator),
  UToA = cutlink_to_actuator(FromNeuronId, ToA, Generation),
  db:write(UToA, actuator).

%%-----------------------------------------------------------------------------
%% @doc The add_outlink function reads the cortex record from the database
%%      based on the cortex id extracted from the agent record. The function
%%      then selects a random neuron from the neuron_ids stored in the cortex
%%      record. The function then subtracts the neuron's output_ids from the
%%      combined list of the actuator and neuron ids belonging to the neural
%%      network to get a list of ids belonging to the elements to which this
%%      neuron is not yet connected. If this list is empty, the function
%%      exits with error. If the list is not empty, it is given the name
%%      AvailableIds, from which a random id is chosen, and the neuron is
%%      then connected to the element to whom the id belongs. Finally, the
%%      agent's evo_hist list is updated, and the updated agent record is
%%      written to the database.
%% @end
%%-----------------------------------------------------------------------------
-spec add_outlink(models:agent_id()) -> ok.
add_outlink(AgentId) ->
  A = db:read(AgentId, agent),
  CxId = models:get(cx_id, A),
  Cx = db:read(CxId, cortex),
  NIds = models:get(neuron_ids, Cx),
  NId = lists:nth(rand:uniform(length(NIds)), NIds),
  N = db:read(NId, neuron),
  OutputIds = models:get(output_ids, N),
  OutlinkNIdPool = filter_outlink_id_pool(models:get(constraint, A), NId, NIds),
  case OutlinkNIdPool -- OutputIds of
    [] ->
      false; % Neuron already connected to all ids
    AvailableIds ->
      ToId = lists:nth(rand:uniform(length(AvailableIds)), AvailableIds),
      link_from_element_to_element(models:get(generation, A), NId, ToId),
      EvoHist = models:get(evo_hist, A),
      UEvoHist = [{add_outlink, NId, ToId} | EvoHist],
      UA = models:set({evo_hist, UEvoHist}, A),
      db:write(UA, agent)
  end.

%%-----------------------------------------------------------------------------
%% @doc The add_inlink function extracts the list of neuron ids within the
%%      NN, and chooses a random id from this list. The input ids belonging to
%%      the neuron's input_idps list are then subtracted from the combined
%%      neuron and sensor ids belonging to the NN. The result is a list of
%%      element ids from which the neuron is not yet connected. If this list
%%      is empty, the function exits with an error, otherwise the function
%%      chooses a random id from this list and establishes a connection
%%      between the neuron and the element correlated with randomly chosen
%%      id. Finally, the agent's evo_hist list is updated, and the updated
%%      agent is written to database.
%% @end
%%-----------------------------------------------------------------------------
-spec add_inlink(models:agent_id()) -> ok.
add_inlink(AgentId) ->
  A = db:read(AgentId, agent),
  CxId = models:get(cx_id, A),
  Cx = db:read(CxId, cortex),
  NIds = models:get(neuron_ids, Cx),
  SIds = sids(A, Cx),
  NId = lists:nth(rand:uniform(length(NIds)), NIds),
  N = db:read(NId, neuron),
  {SIIds, _SWeightPLists} = lists:unzip(models:get(input_idps, N)),
  {MIIds, _MWeightPLists} = lists:unzip(models:get(input_idps_modulation, N)),
  InlinkNIdPool = filter_inlink_id_pool(models:get(constraint, A), NId, NIds),
  IIds = lists:append(SIIds, MIIds),
  case lists:append(SIds, InlinkNIdPool) -- IIds of
    [] ->
      false; % Neuron already connected to all ids
    AvailableIds ->
      FromId = lists:nth(rand:uniform(length(AvailableIds)), AvailableIds),
      link_from_element_to_element(models:get(generation, A), FromId, NId),
      EvoHist = models:get(evo_hist, A),
      UEvoHist = [{add_inlink, FromId, NId} | EvoHist],
      db:write(models:set({evo_hist, UEvoHist}, A), agent)
  end.

%%-----------------------------------------------------------------------------
%% @doc The function add_neuron creates a new neuron, and connects it to a
%%      randomly selected element in the NN, and from a randomly selected
%%      element in the NN. The function first reads the agent's pattern list,
%%      selects a random layer from the pattern, and then creates a new
%%      neuron id for that layer. Then, a new, unconnected neuron, is created
%%      with that neuron id. From the cortex's neuron_ids list, two random
%%      neuron ids are chosen: FromElementId, and ToElementId,
%%      (they can be the same ids). The function then establishes a
%%      connection from the neuron to To_ElemenId, and to the neuron from
%%      FromElementId. Finally, the cortex's neuron_ids list is updated by
%%      appending to it the id of the newly created neuron, the agent's
%%      evo_hist is updated, and the updated cortex and agent records are
%%      written to database.
%% @end
%%-----------------------------------------------------------------------------
-spec add_neuron(models:agent_id()) -> ok.
add_neuron(AgentId) ->
  A = db:read(AgentId, agent),
  Generation = models:get(generation, A),
  Pattern = models:get(pattern, A),
  CxId = models:get(cx_id, A),
  Cx = db:read(CxId, cortex),
  NIds = models:get(neuron_ids, Cx),
  SIds = sids(A, Cx),
  {TargetLayer, TargetNeuronIds} = lists:nth(rand:uniform(length(Pattern)), Pattern),
  NewNId = {neuron, {TargetLayer, genotype:unique_id()}},
  UNIds = [NewNId | NIds],
  UPattern = lists:keyreplace(TargetLayer, 1, Pattern, {TargetLayer, [NewNId | TargetNeuronIds]}),
  SpecCon = models:get(constraint, A),
  genotype:construct_neuron(CxId, Generation, SpecCon, NewNId, [], []),
  InlinkNIdPool = filter_inlink_id_pool(models:get(constraint, A), NewNId, NIds),
  OutlinkNIdPool = filter_outlink_id_pool(models:get(constraint, A), NewNId, NIds),
  FromElementIdPool = InlinkNIdPool ++ SIds,
  ToElementIdPool = OutlinkNIdPool,
  case (FromElementIdPool == []) or (ToElementIdPool == []) of
    true ->
      false; % InlinkNIdPool or OutlinkNIdPool is empty
    false ->
      FromElementId = lists:nth(rand:uniform(length(FromElementIdPool)), FromElementIdPool),
      ToElementId = lists:nth(rand:uniform(length(ToElementIdPool)), ToElementIdPool),
      link_from_element_to_element(models:get(generation, A), FromElementId, NewNId),
      link_from_element_to_element(models:get(generation, A), NewNId, ToElementId),
      UEvoHist = [{add_neuron, FromElementId, NewNId, ToElementId} | models:get(evo_hist, A)],
      db:write(models:set({neuron_ids, UNIds}, Cx), cortex),
      db:write(models:set([{pattern, UPattern}, {evo_hist, UEvoHist}], A), agent)
  end.

%%-----------------------------------------------------------------------------
%% @doc The function outsplice chooses a random neuron id from the cortex's
%%      neuron_ids list, disconnects it from a randomly chosen id in its
%%      output_ids list, and then reconnects it to the same element through a
%%      newly created neuron. The function first chooses a random neuron N
%%      with the neuron id NId from the cortex's neuron_ids list. Then the
%%      neuron N's output_ids list is extracted, and a new id list OIdPool
%%      is created from the ids in the output_ids list that are located in the
%%      layer after the NId's layer (the ids of elements to whom the NId
%%      forms a feed forward connection). From that sublist of N's output_ids
%%      list, a random OId is chosen, and if the sublist is empty, then the
%%      function exits with an error. Then NId is disconnected from the OId.
%%      The function then creates or extracts a new layer index, NewLI,
%%      located between NId and OId. If there exists a layer between NId
%%      and OId, NewLI is simply that layer, if on the other hand OId's
%%      layer comes immediately after NId's then a new layer is created
%%      between OId and NId, whose layer index is in the middle of the two
%%      elements. A new unconnected neuron is then created in that layer,
%%      with a neuron id NewNId, and connected to the OId, and from the
%%      NId, thus establishing a path from NId to OId through the NewNId.
%%      The cortex's neuron_ids is updated with the NewNId, and the agent's
%%      evo_hist list is updated with the new mutation operator tuple
%%      {outsplice, NId, Newn_Id, OId}. Finally, the updated cortex and agent
%%      are written to database.
%% @end
%%-----------------------------------------------------------------------------
-spec outsplice(models:agent_id()) -> ok.
outsplice(AgentId) ->
  A = db:read(AgentId, agent),
  Generation = models:get(generation, A),
  Pattern = models:get(pattern, A),
  CxId = models:get(cx_id, A),
  Cx = db:read(CxId, cortex),
  NIds = [{neuron, {LI, UId}} || {neuron, {LI, UId}} <- models:get(neuron_ids, Cx)],
  NId = lists:nth(rand:uniform(length(NIds)), NIds),
  N = db:read(NId, neuron),
  {neuron, {LayerIndex, _UId}} = NId,
  % Choose a random neuron in the output_ids for splicing, only forward facing.
  OIdPool = [{OT, {OL, OUId}} || {OT, {OL, OUId}} <- models:get(output_ids, N), OL > LayerIndex],
  case OIdPool of
    [] ->
      false;
    _->
      OId = lists:nth(rand:uniform(length(OIdPool)), OIdPool),
      {_OutputType, {OutputLayerIndex, _OutputUId}} = OId,
      % Create a new Layer, or select an existing one between NId and the OId,
      % and create the new unlinked neuron.
      NewLI = new_li_output(OutputLayerIndex, LayerIndex, Pattern),
      NewNId = {neuron, {NewLI, genotype:unique_id()}},
      SpecCon = models:get(constraint, A),
      genotype:construct_neuron(CxId, Generation, SpecCon, NewNId, [], []),
      % Update pattern.
      UPattern = update_pattern(NewLI, Pattern, NewNId),
      % Disconnect the NId from the OId, and reconnect through NewNId
      cutlink_from_element_to_element(models:get(generation, A), NId, OId),
      link_from_element_to_element(models:get(generation, A), NId, NewNId),
      link_from_element_to_element(models:get(generation, A), NewNId, OId),
      % Updated agent
      EvoHist = models:get(evo_hist, A),
      UEvoHist = [{outsplice, NId, NewNId, OId} | EvoHist],
      UCx = models:set({neuron_ids, [NewNId | models:get(neuron_ids, Cx)]}, Cx),
      db:write(UCx, cortex),
      db:write(models:set([{pattern, UPattern}, {evo_hist, UEvoHist}], A), agent)
  end.

%%-----------------------------------------------------------------------------
%% @doc The get_new_li calculates or creates a new layer index located between
%%      FromLI and ToLI. The function calls get_nextli/3 or get_prev_li/3,
%%      depending on whether the direction of the connection is forward, from
%%      sensors towards actuators (Direction = next) or from actuators towards
%%      sensors (Direction = prev), which is the case when executing an
%%      insplice function, which calculates or creates a new layer between
%%      the NId and one of the ids in its input_idps list. If the FromLI ==
%%      ToLI, the function exits with an error.
%% @end
%%-----------------------------------------------------------------------------
-spec get_new_li(float(), float(), next | prev, [{float(), models:neuron_id()}]) -> float().
get_new_li(LI, LI, _Direction, _Pattern) ->
  LI;
get_new_li(FromLI, ToLI, Direction, Pattern) ->
  case Direction of
    next ->
      get_nextli(Pattern, FromLI, ToLI);
    prev ->
      get_prev_li(lists:reverse(Pattern), FromLI, ToLI)
  end.

%%-----------------------------------------------------------------------------
%% @doc The function add_sensorlink, randomly selects a SId from the
%%      cortex's sensor_ids list, and then establishes from that sensor a
%%      connection to a still unlinked to this sensor, randomly selected
%%      neuron from the cortex's neuron_ids list. The function first selects
%%      a random sensor id SId from the cortex's sensor_ids list. Then a
%%      list of NIds to which SId is not yet connected is calculated by
%%      subtracting from the NIds the SIds fanout_ids list. If the resulting
%%      list is empty, then the function exits with an error since there is no
%%      other neurons to which the sensor can establish a new connection to.
%%      If the list is not empty, then a random neuron id, NId, is selected
%%      from this list, and a connection is established from SId to NId.
%%      Finally, the agent's evo_hist is then updated, and it is written to
%%      database.
%% @end
%%-----------------------------------------------------------------------------
-spec add_sensorlink(models:agent_id()) -> ok.
add_sensorlink(AgentId) ->
  A = db:read(AgentId, agent),
  CxId = models:get(cx_id, A),
  Cx = db:read(CxId, cortex),
  NIds = models:get(neuron_ids, Cx),
  SIds = sids(A, Cx),
  SId = lists:nth(rand:uniform(length(SIds)), SIds),
  S = db:read(SId, sensor),
  case NIds -- models:get(fanout_ids, S) of
    [] ->
      false; % Sensor already connected to all NIds
    AvailableIds ->
      NId = lists:nth(rand:uniform(length(AvailableIds)), AvailableIds),
      link_from_element_to_element(models:get(generation, A), SId, NId),
      EvoHist = models:get(evo_hist, A),
      UEvoHist = [{add_sensorlink, SId, NId} | EvoHist],
      db:write(models:set({evo_hist, UEvoHist}, A), agent)
  end.

%%-----------------------------------------------------------------------------
%% @doc The add_actuatorlink selects a random actuator id AId from the
%%      cortex's actuator_ids list, and then connects AId to randomly
%%      selected neuron to which the AId is not yet connected to. The
%%      function first selects a random actuator id AId from the cortex's
%%      actuator_ids list. Then the function creates a list of neuron ids to
%%      which it is not yet connected by subtracting its fanin_ids list from
%%      the cortex's neuron_ids list. If the resulting id pool is empty, then
%%      the function exits with error. If the resulting id pool is not empty,
%%      a neuron id NId is randomly chosen from this id list, and the
%%      actuator is connected to this randomly chosen neuron. Finally, the
%%      agent's evo_hist is updated, and the updated agent is written to
%%      database.
%% @end
%%-----------------------------------------------------------------------------
-spec add_actuatorlink(models:agent_id()) -> ok.
add_actuatorlink(AgentId) ->
  Agent = db:read(AgentId, agent),
  CxId = models:get(cx_id, Agent),
  Cx = db:read(CxId, cortex),
  NIds = models:get(neuron_ids, Cx),
  AIds = case models:get(encoding_type, Agent) of
    neural ->
      models:get(actuator_ids, Cx);
    substrate ->
      models:get(cep_ids, db:read(models:get(substrate_id, Agent), substrate))
  end,
  AId = lists:nth(rand:uniform(length(AIds)), AIds),
  A = db:read(AId, actuator),
  case NIds -- models:get(fanin_ids, A) of
    [] ->
      false; % Already connected from all NIds
    AvailableIds ->
      NId = lists:nth(rand:uniform(length(AvailableIds)), AvailableIds),
      link_from_element_to_element(models:get(generation, Agent), NId, AId),
      EvoHist = models:get(evo_hist, Agent),
      UEvoHist = [{add_actuatorlink, NId, AId} | EvoHist],
      db:write(models:set({evo_hist, UEvoHist}, Agent), agent)
  end.

%%-----------------------------------------------------------------------------
%% @doc The add_sensor function adds and connects a new sensor to the
%%      neural network, a sensor type to which the NN is not yet connected
%%      from. After retrieving the morphology name from the constraints
%%      record retrieved from the agent, the complete set of available sensors
%%      is retrieved using the morphology:get_sensors/1 function. From this
%%      complete sensor list we subtract the sensor tuples used by the NN
%%      based system, but first we revert those sensor's id and cx_id back
%%      to undefined, since that is what the initial state of the sensor
%%      tuples are. With the NN's sensors ids and cx_ids reverted back to
%%      undefined, they can be subtracted from the complete set of sensors.
%%      If the resulting list is empty, then the function exits with an error.
%%      On the other hand if there resulting list is not empty, then there are
%%      still sensors which the NN is not yet using (though it does not mean
%%      that using the sensors would make the NN better, these sensors might
%%      be simply useless, and hence not previously incorporated during
%%      evolution). From this resulting list we then select a random sensor,
%%      and create for it a unique sensor id NewSId. A random neuron id NId
%%      is then selected from the cortex's neuron_ids list, and a connection
%%      is established from NewSId to the NId. The cortex's sensor_ids is
%%      updated with the new sensor's id, and the agent's evo_hist is updated
%%      with the new tuple. The updated cortex and agent records are then
%%      written to database.
%% @end
%%-----------------------------------------------------------------------------
-spec add_sensor(models:agent_id()) -> ok.
add_sensor(AgentId) ->
  Agent = db:read(AgentId, agent),
  CxId = models:get(cx_id, Agent),
  Cx = db:read(CxId, cortex),
  SIds = models:get(sensor_ids, Cx),
  SpecCon = models:get(constraint, Agent),
  Morphology = models:get(morphology, SpecCon),
  case morphology:get_sensors(Morphology) -- [models:set(
    [{id, undefined}, {cx_id, undefined}, {fanout_ids, []}, {generation, undefined}],
    db:read(SId, sensor)) || SId <- SIds] of
      [] ->
        false;
      AvailableSensors ->
        NewSId = {sensor, {-1.0, genotype:unique_id()}},
        NewSensor = models:set([{id, NewSId}, {cx_id, CxId}],
          lists:nth(rand:uniform(length(AvailableSensors)), AvailableSensors)),
        EvoHist = models:get(evo_hist, Agent),
        UEvoHist = case models:get(encoding_type, Agent) of
          neural ->
            db:write(NewSensor, sensor),
            NIds = models:get(neuron_ids, Cx),
            NId = lists:nth(rand:uniform(length(NIds)), NIds),
            link_from_element_to_element(models:get(generation, Agent), NewSId, NId),
            [{add_sensor, NewSId, NId} | EvoHist];
          substrate ->
            SubstrateId = models:get(substrate_id, Agent),
            db:write(models:set({fanout_ids, [SubstrateId]}, NewSensor), sensor),
            [{add_sensor, NewSId, SubstrateId} | EvoHist]
        end,
        UCx = models:set({sensor_ids, [NewSId | SIds]}, Cx),
        db:write(UCx, cortex),
        db:write(models:set({evo_hist, UEvoHist}, Agent), agent)
  end.

%%-----------------------------------------------------------------------------
%% @doc The add_actuator function adds and connects a new actuator to the
%%      neural network, an actuator type to which the NN is not yet
%%      connected to. After the morphology name from the constraints record,
%%      a complete actuator list available to the NN from which to draw its
%%      actuators from during evolution is created. From that list the
%%      actuator list that the NN is already connected to is subtracted,
%%      after the ids and cx_ids of those actuators is set to undefined.
%%      The resulting list is the list of actuators to which the NN is not
%%      yet connected to. A random actuator is chosen from that list, and a
%%      random neuron id NId from cortex's neuron_ids is chosen and
%%      connected to the new actuator. The cortex's actuator_ids list is then
%%      updated with the id of the newly created actuator, the agent's
%%      evo_hist is updated with the new tuple, and then both the updated
%%      cortex and the agent are written to database.
%% @end
%%-----------------------------------------------------------------------------
-spec add_actuator(models:agent_id()) -> ok.
add_actuator(AgentId) ->
  Agent = db:read(AgentId, agent),
  CxId = models:get(cx_id, Agent),
  Cx = db:read(CxId, cortex),
  AIds = models:get(actuator_ids, Cx),
  SpecCon = models:get(constraint, Agent),
  Morphology = models:get(morphology, SpecCon),
  Generation = models:get(generation, Agent),
  case morphology:get_actuators(Morphology) -- [models:set(
    [{cx_id, undefined}, {id, undefined}, {fanin_ids, []}, {generation, undefined}],
    db:read(AId, actuator)) || AId <- AIds] of
      [] ->
        false;
      AvailableActuators ->
        NewAId = {actuator, {1.0, genotype:unique_id()}},
        NewActuator = models:set([{id, NewAId}, {cx_id, CxId}],
          lists:nth(rand:uniform(length(AvailableActuators)), AvailableActuators)),
        EvoHist = models:get(evo_hist, Agent),
        {UCortex, UEvoHistory} = case models:get(encoding_type, Agent) of
          neural ->
            db:write(NewActuator, actuator),
            NIds = [{neuron, {0.0, genotype:unique_id()}}
            || _ <- lists:seq(1, models:get(vl, NewActuator))],
            [genotype:construct_neuron(CxId, Generation, SpecCon, NId, [], []) || NId <- NIds],
            UCx = models:set(
              [{actuator_ids, [NewAId | AIds]},
              {neuron_ids, lists:append(NIds, models:get(neuron_ids, Cx))}], Cx),
            UEvoHist = [{add_actuator, NIds, NewAId} | EvoHist],
            [genotype:link_neuron(Generation, [lists:nth(rand:uniform(length(NIds)), NIds)],
              NId, [models:get(id, NewActuator)]) || NId <- NIds],
            {UCx, UEvoHist};
          substrate ->
            SubstrateId = models:get(substrate_id, Agent),
            db:write(models:set({fanin_ids, [SubstrateId]}, NewActuator), actuator),
            UCx = models:set({actuator_ids, [NewAId | AIds]}, Cx),
            UEvoHist = [{add_actuator, SubstrateId, NewAId} | EvoHist],
            {UCx, UEvoHist}
        end,
        db:write(UCortex, cortex),
        db:write(models:set({evo_hist, UEvoHistory}, Agent), agent)
  end.

%%-----------------------------------------------------------------------------
%% @doc The add_cpp first checks the encoding of the NN based agent. If the
%%      encoding is neural, it exits the function since the neural encoded NN
%%      based system does not use substrate_cpps. If the agent is substrate
%%      encoded, then the function chooses randomly a still unused and
%%      available substrate_cpp from the Available_CPPs list, and then links
%%      it to a randomly chosen neuron in the NN. The function then updates
%%      evo_hist list, writes the updated substrate and agent to database,
%%      and returns to the caller.
%% @end
%%-----------------------------------------------------------------------------
-spec add_cpp(models:agent_id()) -> ok.
add_cpp(AgentId) ->
  Agent = db:read(AgentId, agent),
  case models:get(encoding_type, Agent) of
    neural ->
      false;
    substrate ->
      CxId = models:get(cx_id, Agent),
      Cx = db:read(CxId, cortex),
      SubstrateId = models:get(substrate_id, Agent),
      Substrate = db:read(SubstrateId, substrate),
      Dimensions = length(models:get(densities, Substrate)),
      Plasticity = models:get(plasticity, Substrate),
      CPPIds = models:get(cpp_ids, Substrate),
      SubstrateCPPs = morphology:get_substrate_cpps(Dimensions, Plasticity),
      Sensors = [models:set([{id, undefined}, {cx_id, undefined}, {fanout_ids, []},
        {generation, undefined}], db:read(CPPId, sensor)) || CPPId <- CPPIds],
      case SubstrateCPPs -- Sensors of
        [] ->
          false; % Using all available substrate_cpps
        AvailableCPPs ->
          NewCPPId = {sensor, {-1.0, genotype:unique_id()}},
          NewCPP = models:set([{id, NewCPPId}, {cx_id, CxId}],
            (lists:nth(rand:uniform(length(AvailableCPPs)), AvailableCPPs))),
          EvoHist = models:get(evo_hist, Agent),
          db:write(NewCPP, sensor),
          NIds = models:get(neuron_ids, Cx),
          NId = lists:nth(rand:uniform(length(NIds)), NIds),
          link_from_element_to_element(models:get(generation, Agent), NewCPPId, NId),
          UEvoHist = [{add_cpp, NewCPPId, NId} | EvoHist],
          USubstrate = models:set({cpp_ids, [NewCPPId | CPPIds]}, Substrate),
          db:write(USubstrate, substrate),
          db:write(models:set({evo_hist, UEvoHist}, Agent), agent)
      end
  end.

%%-----------------------------------------------------------------------------
%% @doc The add_cep first checks the encoding of the NN based agent. If the
%%      encoding is neural, it exits the function since the neural encoded NN
%%      based system does not use substrate_ceps. If the agent is substrate
%%      encoded, then the function chooses randomly a still unused and
%%      available substrate_cep from the Available_CEPs list, and then links
%%      it to a randomly chosen neuron in the NN. The function then updates
%%      evo_hist list, writes the updated substrate and agent to database,
%%      and returns to the caller.
%% @end
%%-----------------------------------------------------------------------------
 -spec add_cep(models:agent_id()) -> ok.
add_cep(AgentId) ->
  Agent = db:read(AgentId, agent),
  SpecCon = models:get(constraint, Agent),
  Generation = models:get(generation, Agent),
  case models:get(encoding_type, Agent) of
    neural ->
      false;
    substrate ->
      CxId = models:get(cx_id, Agent),
      Cx = db:read(CxId, cortex),
      SubstrateId = models:get(substrate_id, Agent),
      Substrate = db:read(SubstrateId, substrate),
      Dimensions = length(models:get(densities, Substrate)),
      Plasticity = models:get(plasticity, Substrate),
      CEPIds = models:get(cep_ids, Substrate),
      SubstrateCEPs = morphology:get_substrate_ceps(Dimensions, Plasticity),
      Actuators = [models:set([{id, undefined}, {cx_id, undefined}, {fanin_ids, []},
        {generation, undefined}], db:read(CEPId, actuator)) || CEPId <- CEPIds],
      case SubstrateCEPs -- Actuators of
        [] ->
          false; % Using all available substrate_cpps
        AvailableCEPs ->
          NewCEPId = {actuator, {1.0, genotype:unique_id()}},
          NewCEP = models:set([{id, NewCEPId}, {cx_id, CxId}],
            (lists:nth(rand:uniform(length(AvailableCEPs)), AvailableCEPs))),
          EvoHist = models:get(evo_hist, Agent),
          db:write(NewCEP, actuator),
          NIds = [{neuron, {0.0, genotype:unique_id()}}
          || _ <- lists:seq(1, models:get(vl, NewCEP))],
          [genotype:construct_neuron(CxId, Generation, SpecCon, NId, [], [])|| NId <- NIds],
          UEvoHist = [{add_cep, NIds, NewCEPId} | EvoHist],
          [genotype:link_neuron(
            Generation, [lists:nth(rand:uniform(length(NIds)), NIds)],
            NId, [models:get(id, NewCEP)]) || NId <- NIds
          ],
          UCx = models:set({neuron_ids, lists:append(NIds, models:get(neuron_ids, Cx))}, Cx),
          db:write(UCx, cortex),
          USubstrate = models:set({cep_ids, [NewCEPId | CEPIds]}, Substrate),
          db:write(USubstrate, substrate),
          db:write(models:set({evo_hist, UEvoHist}, Agent), agent)
      end
  end.

%%%============================================================================
%%% Internal functions
%%%============================================================================

sids(A, Cx) ->
  case models:get(encoding_type, A) of
    neural ->
      models:get(sensor_ids, Cx);
    substrate ->
      models:get(cpp_ids, db:read(models:get(substrate_id, A), substrate))
  end.

%%-----------------------------------------------------------------------------
%% @private
%% @doc The mutate_search_paramters with a probability of
%%      ?SEARCH_PARAMETERS_MUTATION_PROBABILITY applies a random number
%%      between 1 and length(?ESMUTATORS) of evolutionary strategy mutation
%%      operators from the ?ESMUTATORS list.
%% @end
%%-----------------------------------------------------------------------------
mutate_search_parameters(AgentId) ->
  SearchParamsMutProb = app_config:get_env(search_params_mut_prob),
  case rand:uniform() < SearchParamsMutProb of
    true ->
      TotMutations = rand:uniform(length(?ESMUTATORS)),
      apply_esmutators(AgentId, TotMutations);
    false ->
      ok
  end.

%%-----------------------------------------------------------------------------
%% @private
%% @doc apply_esmutators with uniform distribution chooses a random
%%      evolutionary strategy mutation operator from the ?ESMUTATORS list of
%%      such functions, and applies it to the agent. Whether the mutation is
%%      successful or not, the function counts down the total number of
%%      mutation operators left to apply. This is to ensure that if the
%%      researcher set for each such evolutionary strategy to be static,
%%      having only one available mutable parameter for every agent, the
%%      system will eventually try to mutate the strategy TotMutations number
%%      of times, and then return to the caller.
%% @end
%%-----------------------------------------------------------------------------
apply_esmutators(_AgentId, 0) ->
  ok;
apply_esmutators(AgentId, MutationIndex) ->
  ESMutators = ?ESMUTATORS,
  ESMutator = lists:nth(rand:uniform(length(ESMutators)), ESMutators),
  case genome_mutator:ESMutator(AgentId) of
    ok ->
      logr:debug({genome_mutator, apply_esmutators, ok,
        "evolutionary strategy mutation succeeded", [ESMutator]}),
      apply_esmutators(AgentId, MutationIndex - 1);
    _Error ->
      logr:debug({genome_mutator, apply_esmutators, ok,
        "evolutionary strategy mutation canceled. retrying with new mutation", []}),
      apply_esmutators(AgentId, MutationIndex - 1)
  end.

%%-----------------------------------------------------------------------------
%% @private
%% @doc The apply_mutators applies the set number of successful mutation
%%      operators to the Agent. If a mutation operator exits with an error,
%%      the function tries another mutation operator. It is only after a
%%      successful mutation operator is applied that the MutationIndex is
%%      decremented.
%% @end
%%-----------------------------------------------------------------------------
apply_mutators(_AgentId, 0) ->
  done;
apply_mutators(AgentId, MutationIndex) ->
  Result = apply_neural_mutator(AgentId),
  case Result of
    ok ->
      logr:debug({genome_mutator, apply_mutators, ok, "mutation succeeded", []}),
      apply_mutators(AgentId, MutationIndex -1);
    _Error ->
      logr:debug({genome_mutator, apply_mutators, ok,
        "mutation canceled. retrying with new mutation", []}),
      apply_mutators(AgentId, MutationIndex)
  end.

%%-----------------------------------------------------------------------------
%% @private
%% @doc The apply_neural_mutator applies the available mutation operators to
%%      the NN. Because the genotype is stored in the DB, if the mutation
%%      operator function exits with an error, the database made changes are
%%      retracted, and a new mutation operator can then be applied to the
%%      agent, as if the previous unsuccessful mutation operator was never
%%      applied. The mutation operator to be applied to the agent is chosen
%%      randomly from the agent's mutation_operators list.
%% @end
%%-----------------------------------------------------------------------------
apply_neural_mutator(AgentId) ->
  A = db:read(AgentId, agent),
  MutatorsP = models:get(mutation_operators, A),
  Mutator = select_random_mo(MutatorsP),
  genome_mutator:Mutator(AgentId).

%%-----------------------------------------------------------------------------
%% @private
%% @doc The select_random_mo, using the analogy of a roulette wheel, first
%%      calculates the entire area of the wheel by summing together all the
%%      slice sizes of the parts. The function than chooses randomly a spot
%%      on the wheel, and through select_random_mo/3 calculates where that
%%      spot is located, with regards to the mutation operator that it falls
%%      on. Since some slices are larger then others, they will have uniformly
%%      larger probabilities of being selected.
%% @end
%%-----------------------------------------------------------------------------
select_random_mo(MutatorsP) ->
  TotSize = lists:sum([SliceSize || {_MO, SliceSize} <- MutatorsP]),
  Choice = rand:uniform(TotSize),
  select_random_mo(MutatorsP, Choice, 0).

select_random_mo([{MO, SliceSize} | MOs], Choice, RangeFrom) ->
  RangeTo = RangeFrom + SliceSize,
  case (Choice >= RangeFrom) and (Choice =< RangeTo) of
    true ->
      MO;
    false ->
      select_random_mo(MOs, Choice, RangeTo)
  end;
select_random_mo([], _Choice, _RangeFrom) ->
  exit("select_random_mo :: reached [] without selecting a mutation operator.").

mutate_weights(NId, Spread) ->
  N = db:read(NId, neuron),
  InputIdPs = models:get(input_idps, N),
  InputMIdPs = models:get(input_idps_modulation, N),
  PF = models:get(pf, N),
  PerturbedSIPidPs = perturb_ipidps(Spread, InputIdPs),
  PerturbedMIPidPs = perturb_ipidps(Spread, InputMIdPs),
  PerturbedPF = perturb_pf(Spread, PF),
  UN = models:set([{input_idps, PerturbedSIPidPs},
    {input_idps_modulation, PerturbedMIPidPs}, {pf, PerturbedPF}], N),
  db:write(UN, neuron).

perturb_ipidps(_Spread, []) ->
  [];
perturb_ipidps(Spread, InputIdPs) ->
  TotWeights = lists:sum([length(WeightsP) || {_InputId, WeightsP} <- InputIdPs]),
  MP = 1 / math:sqrt(TotWeights),
  perturb_ipidps(Spread, MP, InputIdPs, []).

perturb_ipidps(Spread, MP, [{InputId, WeightsP} | InputIdPs], Acc) ->
  UWeightsP = case rand:uniform() < MP of
    true ->
      perturb_weights_p(Spread, 1 / math:sqrt(length(WeightsP)), WeightsP, []);
    false ->
      WeightsP
  end,
  perturb_ipidps(Spread, MP, InputIdPs, [{InputId, UWeightsP} | Acc]);
perturb_ipidps(_Spread, _MP, [], Acc) ->
  lists:reverse(Acc).

%%-----------------------------------------------------------------------------
%% @private
%% @doc The link_from_neuron updates the record of the neuron from whom the
%%      link is being created. FromN is the record of the neuron from whom the
%%      link/connection emanates, and ToId is the id of the element to whom
%%      the link is headed towards. The function extracts the layer index of
%%      the neuron FromN, and the layer index of the element with the id ToId.
%%      Then the two layer indices's are compared, and the ToId is either added
%%      only to the FromN's output_ids list, or if the connection is
%%      recursive, ToLayerIndex =< FromLayerIndex, to output_ids and ro_ids
%%      lists. The FromN's generation is updated to the value Generation,
%%      which is the current, most recent generation, since this neuron has
%%      just been modified. Finally, the updated neuron record is then
%%      returned to the caller. On the other hand, if ToId, the id of the
%%      element to which the connection is being established, is already a
%%      member of the FromN's output_ids list, then the function exits with
%%      error.
%% @end
%%-----------------------------------------------------------------------------
link_from_neuron(FromN, ToId, Generation) ->
  {_, {FromLI, _}} = models:get(id, FromN),
  {_, {ToLI, _}} = ToId,
  FromOutputIds = models:get(output_ids, FromN),
  FromROIds = models:get(ro_ids, FromN),
  case lists:member(ToId, FromOutputIds) of
    true ->
      exit("add_neuronO [can not add OId to Neuron] - Already member~n");
    false ->
      {UFromOutputIds, UFromROIds} = case FromLI >= ToLI of
        true ->
          {[ToId | FromOutputIds], [ToId | FromROIds]};
        false ->
          {[ToId | FromOutputIds], FromROIds}
      end,
      models:set([{output_ids, UFromOutputIds}, {ro_ids, UFromROIds},
        {generation, Generation}], FromN)
  end.

%%-----------------------------------------------------------------------------
%% @private
%% @doc The link_to_neuron updates the record of ToN, so that its updated to
%%      receive a connection from the element FromId. The link emanates from
%%      element with the id FromId, whose output vector length is FromOVL, and
%%      the connection is made to the neuron ToN, the record which is updated
%%      in this function. Randomly chosen, either the ToN's
%%      input_idps_modulation or input_idps list is updated with the tuple
%%      {FromId, [{W_1, WPs}...{W_FromOVL, WPs}]}, then the neuron's generation
%%      is updated to Generation (the current, most recent generation), and
%%      the updated ToN's record is returned to the caller. On the other hand,
%%      if the FromId is already part of the ToN's input_idps or
%%      input_idps_modulation list (dependent on which was randomly chosen),
%%      which means that the standard or modulatory link already exists
%%      between the neuron ToN and element FromId, the the function exits
%%      with an error.
%% @end
%%-----------------------------------------------------------------------------
link_to_neuron(FromId, FromOVL, ToN, Generation) ->
  ToSIIdPs = models:get(input_idps, ToN),
  ToMIIdPs = models:get(input_idps_modulation, ToN),
  {PFName, _NLParameters} = models:get(pf, ToN),
  case {lists:keymember(FromId, 1, ToSIIdPs), lists:keymember(FromId, 1, ToMIIdPs)} of
    {false, false} ->
      case {PFName == neuromodulation, rand:uniform(2)} of
        {true, 2} ->
          UToMIIdPs = updated_to_iidps(FromId, PFName, FromOVL, ToMIIdPs),
          models:set([{input_idps_modulation, UToMIIdPs}, {generation, Generation}], ToN);
        _ ->
          UToSIIdPs = updated_to_iidps(FromId, PFName, FromOVL, ToSIIdPs),
          models:set([{input_idps, UToSIIdPs}, {generation, Generation}], ToN)
      end;
    _ ->
      exit("add_NeuronI :: [can not add IId] - Already connected ~n")
  end.

%%-----------------------------------------------------------------------------
%% @private
%% @doc The function link_from_sensor updates the record of the sensor FromS,
%%      from whom the link emanates towards the element with id ToId. First
%%      the function ensures that there is no connection yet established
%%      between FromS and ToId, if a connection between these two elements
%%      already exists, then the function exits with error. If there is no
%%      connection between the two elements, then ToId is added to the
%%      sensor's fanout_ids list, and the updated record of the sensor is
%%      returned to the caller.
%% @end
%%-----------------------------------------------------------------------------
link_from_sensor(FromS, ToId, Generation) ->
  FromFanoutIds = models:get(fanout_ids, FromS),
  case lists:member(ToId, FromFanoutIds) of
    true ->
      exit("link_from_sensor [cant add ToId to Sensor] - Already member ~n");
    false ->
      models:set([{fanout_ids, [ToId | FromFanoutIds]}, {generation, Generation}], FromS)
  end.

get_ivl([Id | Ids], Acc) ->
  VL = case Id of
    {neuron, _} ->
      1;
    {sensor, _} ->
      S = db:read(Id, sensor),
      models:get(vl, S)
  end,
  get_ivl(Ids, Acc + VL);
get_ivl([], Acc) ->
  Acc.

%%-----------------------------------------------------------------------------
%% @private
%% @doc The cutlink_from_neuron cuts the connection on the FromNeuron (FromN)
%%      side. The function first checks if the ToId is a member of the
%%      output_ids list, if its not then the function exits with an error. If
%%      the ToId is a member of the output_ids list, then the function removes
%%      the ToId from the FromOutputIds and from the FromROIds. Even if the
%%      ToId is a recursive connection, then removing it from ro_ids updates
%%      the FromROIds list, if its not, then no change is made to the ro_ids
%%      list. Once the lists are updated, the updated neuron record of FromN
%%      is returned to the caller.
%% @end
%%-----------------------------------------------------------------------------
cutlink_from_neuron(FromN, ToId, Generation) ->
  FromOutputIds = models:get(output_ids, FromN),
  FromROIds = models:get(ro_ids, FromN),
  case lists:member(ToId, FromOutputIds) of
    true ->
      UFromOutputIds = FromOutputIds -- [ToId],
      UFromROIds = FromROIds -- [ToId], %Not necessary if not recursive...
      models:set([{output_ids, UFromOutputIds},
        {ro_ids, UFromROIds}, {generation, Generation}], FromN);
    false ->
      exit("cutlink_from_neuron [can not remove OId] - Not a member ~n")
  end.

%%-----------------------------------------------------------------------------
%% @private
%% @doc The cutlink_to_neuron cuts the connection on the ToNeuron (ToN) side.
%%      The function first checks if the FromId is a member of the ToN's
%%      input_idps list, if its not, then the function checks if it is a
%%      member of the input_idps_modulation list. If it is not a member of
%%      either, the function exits with error. If FromId is a member of one
%%      of these lists, then that tuple is removed from that list, and the
%%      updated ToN record is returned to the caller.
%% @end
%%-----------------------------------------------------------------------------
cutlink_to_neuron(FromId, ToN, Generation) ->
  ToSIIdPs = models:get(input_idps, ToN),
  ToMIIdPs = models:get(input_idps_modulation, ToN),
  case lists:keymember(FromId, 1, ToSIIdPs) of
    true ->
      UToSIIdPs = lists:keydelete(FromId, 1, ToSIIdPs),
      models:set([{input_idps, UToSIIdPs}, {generation, Generation}], ToN);
    false ->
      case lists:keymember(FromId, 1, ToMIIdPs) of
        true ->
          UToMIIdPs = lists:keydelete(FromId, 1, ToMIIdPs),
          models:set([{input_idps_modulation, UToMIIdPs}, {generation, Generation}], ToN);
        false ->
          exit("cutlink_to_neuron [can not remove IId] - Not a member ~n")
      end
  end.

%%-----------------------------------------------------------------------------
%% @private
%% @doc The cutlink_from_sensor function first checks whether ToId is a
%%      member of the sensor's FromS fanout_ids list. If its not, then the
%%      function exits with an error. If ToId is a member of FromS's
%%      fanout_ids list, then it is removed from that list, and the updated
%%      sensor record of FromS is returned to the caller.
%% @end
%%-----------------------------------------------------------------------------
cutlink_from_sensor(FromS, ToId, Generation) ->
  FromFanoutIds = models:get(fanout_ids, FromS),
  case lists:member(ToId, FromFanoutIds) of
    true ->
      UFromFanoutIds = FromFanoutIds -- [ToId],
      models:set([{fanout_ids, UFromFanoutIds}, {generation, Generation}], FromS);
    false ->
      exit("cutlink_from_sensor [can't remove ToId] - Not a member ~n")
  end.

%%-----------------------------------------------------------------------------
%% @private
%% @doc The cutlink_to_actuator function cuts the connection on the
%%      ToActuator's side. The function first checks if the FromId is a member
%%      of the actuator ToA's fanin_ids list. If its not, the function exits
%%      with an error. If FromId is a member of the actuator's fanin_ids list,
%%      then the id is removed from the list, and the updated actuator record
%%      is returned to the caller.
%% @end
%%-----------------------------------------------------------------------------
cutlink_to_actuator(FromId, ToA, Generation) ->
  ToFaninIds = models:get(fanin_ids, ToA),
  case lists:member(FromId, ToFaninIds) of
    true ->
      UToFaninIds = ToFaninIds -- [FromId],
      models:set([{fanin_ids, UToFaninIds}, {generation, Generation}], ToA);
    false ->
      exit("cutlink_to_actuator [can't remove FromId] - Not a member~n")
  end.

%%-----------------------------------------------------------------------------
%% @private
%% @doc The function filter_outlink_id_pool uses the connection_architecture
%%      specification in the constraint record of the agent to return a
%%      filtered neuron id pool. For the feedforward connection_architecture,
%%      the function ensures that only the neurons in the forward facing
%%      layers are allowed in the id pool.
%% @end
%%-----------------------------------------------------------------------------
filter_outlink_id_pool(C, NId, NIds) ->
  case models:get(connection_architecture, C) of
    recurrent ->
      NIds;
    feedforward ->
      {neuron, {LI, _}} = NId,
      [{neuron, {OutlinkLI, OutlinkUniqueId}} || {neuron, {OutlinkLI, OutlinkUniqueId}}
      <- NIds, OutlinkLI > LI]
  end.

%%-----------------------------------------------------------------------------
%% @private
%% @doc The function filter_inlink_id_pool uses the connection_architecture
%%      specification in the constraint record of the agent to return a
%%      filtered neuron id pool. For the feedforward connection_architecture,
%%      the function ensures that only the neurons in the previous layers
%%      are allowed in the filtered neuron id pool.
%% @end
%%-----------------------------------------------------------------------------
filter_inlink_id_pool(C, NId, NIds) ->
  case models:get(connection_architecture, C) of
    recurrent ->
      NIds;
    feedforward ->
      {neuron, {LI, _}} = NId,
      [{neuron, {InlinkLI, InlinkUniqueId}} || {neuron, {InlinkLI, InlinkUniqueId}}
      <- NIds, InlinkLI < LI]
  end.

%%-----------------------------------------------------------------------------
%% @private
%% @doc The get_nextli checks whether the ToLI comes directly after FromLI,
%%      or whether there is another layer between them. If there is another
%%      layer between them, then that layer is returned, and the splice
%%      neuron should then be put into that layer. If there is no layer
%%      between FromLI and ToLI, then a new layer is created in the middle,
%%      the new layer index has the value of (FromLI+ToLI) / 2.
%% @end
%%-----------------------------------------------------------------------------
get_nextli([{FromLI, _LastLayerNIds}], FromLI, ToLI) ->
  (FromLI + ToLI) / 2;
get_nextli([{LI, _LayerNIds} | Pattern], FromLI, ToLI) ->
  case LI == FromLI of
    true ->
      next_li(Pattern, ToLI, FromLI);
    false ->
      get_nextli(Pattern, FromLI, ToLI)
  end.

%%-----------------------------------------------------------------------------
%% @private
%% @doc The get_prev_li checks whether the The ToLI comes directly before
%%      FromLI, or whether there is another layer in between them. If there
%%      is another layer, then the function returns that layer, if no such
%%      layer is found, the the function creates a new layer index,
%%      (FromLI+ToLI) / 2.
%% @end
%%-----------------------------------------------------------------------------
get_prev_li([{FromLI, _FirstLayerNIds}], FromLI, ToLI) ->
  (FromLI + ToLI) / 2;
get_prev_li([{LI, _LayerNIds} | Pattern], FromLI, ToLI) ->
  case LI == FromLI of
    true ->
      next_li(Pattern, ToLI, FromLI);
    false ->
      get_prev_li(Pattern, FromLI, ToLI)
  end.

%%-----------------------------------------------------------------------------
%% @private
%% @doc The perturb_idps accepts the InputIdPs list of the
%%      format:[{Id, Weights}...], calculates the total number of weights in
%%      the InputIdPs, and then calculates the mutation probability MP,
%%      which is 1/sqrt(TotWeights). Once the mutation probability is
%%      calculated, each weight in the InputIdPs list has a chance of MP to
%%      be perturbed/mutated. Once all the weights in the InputIdPs list had
%%      a chance of being mutated, the updated InputIdPs is returned to the
%%      caller.
%% @end
%%-----------------------------------------------------------------------------
perturb_idps(InputIdPs) ->
  TotWeightsP = lists:sum([length(WeightsP) || {_InputId, WeightsP} <- InputIdPs]),
  MP = 1 / math:sqrt(TotWeightsP),
  perturb_idps(MP, InputIdPs, []).

perturb_idps(MP, [{InputId, WeightsP} | InputIdPs], Acc) ->
  UWeightsP = perturb_weights_p(?DELTA_MULTIPLIER, MP, WeightsP, []),
  perturb_idps(MP, InputIdPs, [{InputId, UWeightsP} | Acc]);
perturb_idps(_MP, [], Acc) ->
  lists:reverse(Acc).

%%----------------------------------------------------------------------------
%% @private
%% @doc The perturb_weights_p function goes through each weights block and
%%      calls the perturb_weights_p to perturb the weights.
%% @end
%%----------------------------------------------------------------------------
perturb_weights_p(Spread, MP, [{W, LPs} | Weights], Acc) ->
  UW = case rand:uniform() < MP of
    true ->
      sat((rand:uniform() -0.5) * 2 * Spread + W, -?SAT_LIMIT, ?SAT_LIMIT);
    false ->
      W
  end,
  perturb_weights_p(Spread, MP, Weights, [{UW, LPs} | Acc]);
perturb_weights_p(_Spread, _MP, [], Acc) ->
  lists:reverse(Acc).

new_tsf(TuningSelectionFunctions) ->
  lists:nth(rand:uniform(length(TuningSelectionFunctions)), TuningSelectionFunctions).

check_bias(SIIdPs, MIIdPs, PFName) ->
  {lists:keymember(bias, 1, SIIdPs), lists:keymember(bias, 1, MIIdPs),
  PFName == neuromodulation, rand:uniform(2)}.

do_add_bias(IIdPs, PFName) ->
  lists:append(IIdPs,
    [{bias, [{rand:uniform() - 0.5, plasticity:PFName(weight_parameters)}]}]).

new_li_output(OutputLayerIndex, LayerIndex, Pattern) ->
  case OutputLayerIndex >= LayerIndex of
    true ->
      get_new_li(LayerIndex, OutputLayerIndex, next, Pattern);
    false ->
      get_new_li(LayerIndex, OutputLayerIndex, prev, Pattern)
  end.

update_pattern(NewLI, Pattern, NewNId) ->
  case lists:keymember(NewLI, 1, Pattern) of
    true ->
      {NewLI, InLayerIds} = lists:keyfind(NewLI, 1, Pattern),
      lists:keyreplace(NewLI, 1, Pattern, {NewLI, [NewNId | [InLayerIds]]});
    false ->
      lists:sort([{NewLI, [NewNId]} | Pattern])
  end.

updated_to_iidps(FromId, PFName, FromOVL, ToIIdPs) ->
  [{FromId, genotype:create_neural_weights_p(PFName, FromOVL, [])} | ToIIdPs].

next_li(Pattern, ToLI, FromLI) ->
  [{LI, _LayerNIds} | _] = Pattern,
  case LI == ToLI of
    true ->
      (FromLI + ToLI) / 2;
    false ->
      LI / 1
  end.

perturb_pf(Spread, {PFName, PFParameters}) ->
  UPFParameters = [sat(PFParameter + (rand:uniform() - 0.5) * Spread, -?SAT_LIMIT, ?SAT_LIMIT) ||
  PFParameter <- PFParameters],
  {PFName, UPFParameters}.

sat(Val, Min, _Max) when Val < Min ->
  Min;
sat(Val, _Min, Max) when Val > Max ->
  Max;
sat(Val, _Min, _Max) ->
  Val.