-module(genome_mutator_test).

-include_lib("eunit/include/eunit.hrl").

%% macros

-define(AFS, [tanh, cos, gaussian, absolute, sin, sqrt, sigmoid]).
-define(CXID, {cortex, {origin, 11.67818}}).
-define(N1ID, {neuron, {0.0, 1.2131105277401442}}).
-define(N2ID, {neuron, {0.0, 2.2719130909152305}}).
-define(N3ID, {neuron, {0.0, 1.34564564128163}}).
-define(N4ID, {neuron, {0.0, 1.159246038128163}}).
-define(AGENT_ID, {agent, 24.462463242343}).
-define(AGENT_ID1, {agent, 69.623513234324}).
-define(AGENT_ID2, {agent, 222.65344234234}).
-define(S1ID, {sensor, {-1.0, 234.2343}}).
-define(S2ID, {sensor, {-1.0, 657.3456}}).
-define(A1ID, {actuator, {1.0, 56.6666}}).
-define(A2ID, {actuator, {1.0, 77.4444}}).

%% runners

genome_mutator_test_() ->
  {setup,
    fun setup/0,
    fun cleanup/1,
  [
    fun mutate_subtest/0,
    fun mutate_tuning_selection_subtest/0,
    fun mutate_tuning_annealing_subtest/0,
    fun mutate_tot_topological_mutations_subtest/0,
    fun mutate_heredity_type_subtest/0,
    fun mutate_weights_subtest/0,
    fun add_bias_subtest/0,
    fun remove_bias_subtest/0,
    fun mutate_af_subtest/0,
    fun mutate_pf_subtest/0,
    fun mutate_plasticity_parameters_subtest/0,
    fun mutate_aggr_f_subtest/0,
    fun link_from_element_to_element_subtest/0,
    fun link_from_neuron_to_neuron_subtest/0,
    fun link_from_sensor_to_neuron_subtest/0,
    fun link_from_neuron_to_actuator_subtest/0,
    fun cutlink_from_element_to_element_subtest/0,
    fun cutlink_from_neuron_to_neuron_subtest/0,
    fun cutlink_from_sensor_to_neuron_subtest/0,
    fun cutlink_from_neuron_to_actuator_subtest/0,
    fun add_outlink_subtest/0,
    fun add_inlink_subtest/0,
    fun add_neuron_subtest/0,
    fun add_actuatorlink_subtest/0,
    fun outsplice_subtest/0,
    fun get_new_li_subtest/0,
    fun add_sensorlink_subtest/0,
    fun add_sensor_subtest/0,
    fun add_actuator_subtest/0,
    fun add_cpp_subtest/0,
    fun add_cep_subtest/0
  ]}.

%% tests

mutate_subtest() ->
  ?assertEqual(ok, genome_mutator:mutate(?AGENT_ID)).

mutate_tuning_selection_subtest() ->
  ?assertEqual(ok, genome_mutator:mutate_tuning_selection(?AGENT_ID)).

mutate_tuning_annealing_subtest() ->
  ?assertEqual(ok, genome_mutator:mutate_tuning_annealing(?AGENT_ID)).

mutate_tot_topological_mutations_subtest() ->
  ?assertEqual(ok, genome_mutator:mutate_tot_topological_mutations(?AGENT_ID)).

mutate_heredity_type_subtest() ->
  ?assertEqual(ok, genome_mutator:mutate_heredity_type(?AGENT_ID)).

mutate_weights_subtest() ->
  ?assertEqual(ok, genome_mutator:mutate_weights(?AGENT_ID)).

add_bias_subtest() ->
  ?assert(lists:member(genome_mutator:add_bias(?AGENT_ID), [ok, false])).

remove_bias_subtest() ->
  ?assert(lists:member(genome_mutator:remove_bias(?AGENT_ID), [ok, false])).

mutate_af_subtest() ->
  ?assertEqual(ok, genome_mutator:mutate_af(?AGENT_ID)).

mutate_pf_subtest() ->
  ?assertEqual(ok, genome_mutator:mutate_pf(?AGENT_ID)).

mutate_plasticity_parameters_subtest() ->
  ?assertEqual(ok, genome_mutator:mutate_plasticity_parameters(?AGENT_ID)).

mutate_aggr_f_subtest() ->
  ?assertEqual(ok, genome_mutator:mutate_aggr_f(?AGENT_ID)).

link_from_element_to_element_subtest() ->
  ?assertEqual(ok, genome_mutator:link_from_element_to_element(1, ?N1ID, ?N2ID)),
  ?assertEqual(ok, genome_mutator:link_from_element_to_element(1, ?S2ID, ?N3ID)).

link_from_neuron_to_neuron_subtest() ->
  ?assertEqual(ok, genome_mutator:link_from_neuron_to_neuron(1, ?N3ID, ?N4ID)).

link_from_sensor_to_neuron_subtest() ->
  ?assertEqual(ok, genome_mutator:link_from_sensor_to_neuron(1, ?S1ID, ?N4ID)).

link_from_neuron_to_actuator_subtest() ->
  ?assertEqual(ok, genome_mutator:link_from_neuron_to_actuator(1, ?N2ID, ?A2ID)).

cutlink_from_element_to_element_subtest() ->
  ?assertEqual(ok, genome_mutator:cutlink_from_element_to_element(1, ?N1ID, ?N2ID)).

cutlink_from_neuron_to_neuron_subtest() ->
  ?assertEqual(ok, genome_mutator:cutlink_from_neuron_to_neuron(1, ?N3ID, ?N4ID)).

cutlink_from_sensor_to_neuron_subtest() ->
  ?assertEqual(ok, genome_mutator:cutlink_from_sensor_to_neuron(1, ?S2ID, ?N3ID)).

cutlink_from_neuron_to_actuator_subtest() ->
  ?assertEqual(ok, genome_mutator:cutlink_from_neuron_to_actuator(1, ?N2ID, ?A2ID)).

add_outlink_subtest() ->
  ?assertEqual(ok, genome_mutator:add_outlink(?AGENT_ID)).

add_inlink_subtest() ->
  ?assertEqual(ok, genome_mutator:add_inlink(?AGENT_ID)).

add_neuron_subtest() ->
  ?assertEqual(ok, genome_mutator:add_neuron(?AGENT_ID)).

add_actuatorlink_subtest() ->
  case genome_mutator:add_actuatorlink(?AGENT_ID) of
    false ->
      true;
    ok ->
      true
  end.

outsplice_subtest() ->
  case genome_mutator:outsplice(?AGENT_ID) of
    false ->
      true;
    ok ->
      true
  end.

get_new_li_subtest() ->
  A = db:read(?AGENT_ID, agent),
  Pattern = models:get(pattern, A),
  ?assertEqual(1.0, genome_mutator:get_new_li(1.0, 1.0, Pattern, next)).

add_sensorlink_subtest() ->
  case genome_mutator:add_sensorlink(?AGENT_ID) of
    false ->
      true;
    ok ->
      true
  end.

add_sensor_subtest() ->
  case genome_mutator:add_sensor(?AGENT_ID) of
    false ->
      true;
    ok ->
      true
  end.

add_actuator_subtest() ->
  case genome_mutator:add_actuator(?AGENT_ID) of
    false ->
      true;
    ok ->
      true
  end.

add_cpp_subtest() ->
  case genome_mutator:add_cpp(?AGENT_ID) of
    false ->
      true;
    ok ->
      true
  end.

add_cep_subtest() ->
  case genome_mutator:add_cep(?AGENT_ID) of
    false ->
      true;
    ok ->
      true
  end.

%% helpers

setup() ->
  N1 = models:neuron(#{
    id => ?N1ID,
    generation => 1,
    cx_id => ?CXID,
    af => lists:nth(rand:uniform(length(?AFS)), ?AFS),
    pf => {hebbian, [rand:uniform() - 0.5]},
    aggr_f => dot_product,
    input_idps => [{{neuron, {0.0, rand:uniform() - 0.5}},
    [{rand:uniform() - 0.5, [rand:uniform() - 0.5]}]}],
    input_idps_modulation => [],
    output_ids => [],
    ro_ids => []
  }),
  N2 = models:neuron(#{
    id => ?N2ID,
    generation => 1,
    cx_id => ?CXID,
    af => lists:nth(rand:uniform(length(?AFS)), ?AFS),
    pf => {hebbian, [rand:uniform() - 0.5]},
    aggr_f => dot_product,
    input_idps => [{{neuron, {0.0, rand:uniform() - 0.5}},
    [{rand:uniform() - 0.5, [rand:uniform() - 0.5]}]}],
    input_idps_modulation => [],
    output_ids => [],
    ro_ids => []
  }),
  N3 = models:neuron(#{
    id => ?N3ID,
    generation => 1,
    cx_id => ?CXID,
    af => lists:nth(rand:uniform(length(?AFS)), ?AFS),
    pf => {hebbian, [rand:uniform() - 0.5]},
    aggr_f => dot_product,
    input_idps => [{{neuron, {0.0, rand:uniform() - 0.5}},
    [{rand:uniform() - 0.5, [rand:uniform() - 0.5]}]}],
    input_idps_modulation => [],
    output_ids => [],
    ro_ids => []
  }),
  N4 = models:neuron(#{
    id => ?N4ID,
    generation => 1,
    cx_id => ?CXID,
    af => lists:nth(rand:uniform(length(?AFS)), ?AFS),
    pf => {hebbian, [rand:uniform() - 0.5]},
    aggr_f => dot_product,
    input_idps => [{{neuron, {0.0, rand:uniform() - 0.5}},
    [{rand:uniform() - 0.5, [rand:uniform() - 0.5]}]}],
    input_idps_modulation => [],
    output_ids => [],
    ro_ids => []
  }),

  ok = db:write(N1, neuron),
  ok = db:write(N2, neuron),
  ok = db:write(N3, neuron),
  ok = db:write(N4, neuron),

  S1 = models:sensor(#{
    id => ?S1ID,
    name => {flatland_sensor, distance_scanner},
    type => standard,
    cx_id => ?CXID,
    scape => {public, flatland},
    vl => 5,
    fanout_ids => [],
    generation => 1,
    format => no_geo,
    parameters => [[math:pi()/2], [5], [0.0]]
  }),
  S2 = models:sensor(#{
    id => ?S2ID,
    name => {flatland_sensor, color_scanner},
    type => standard,
    cx_id => ?CXID,
    scape => {public, flatland},
    vl => 5,
    fanout_ids => [],
    generation => 1,
    format => no_geo,
    parameters => [[math:pi()/2], [5], [0.0]]
  }),

  ok = db:write(S1, sensor),
  ok = db:write(S2, sensor),

  A1 = models:actuator(#{
    id => ?A1ID,
    name => {flatland_actuator, two_wheels},
    type => standard,
    cx_id => ?CXID,
    scape => {public, flatland},
    vl => 2,
    fanin_ids => [],
    generation => 1,
    format => no_geo,
    parameters => [2]
  }),
  A2 = models:actuator(#{
    id => ?A2ID,
    name => {flatland_actuator, two_wheels},
    type => standard,
    cx_id => ?CXID,
    scape => {public, flatland},
    vl => 2,
    fanin_ids => [],
    generation => 1,
    format => no_geo,
    parameters => [2]
  }),

  ok = db:write(A1, actuator),
  ok = db:write(A2, actuator),

  application:set_env(apxr_run, build_tool, erlang),
  application:set_env(apxr_run, min_pimprovement, 0.0),
  application:set_env(apxr_run, search_params_mut_prob, 0),
  application:set_env(apxr_run, output_sat_limit, 1),
  application:set_env(apxr_run, ro_signal, [0.0]),
  application:set_env(apxr_run, fitness_stagnation, false),
  application:set_env(apxr_run, re_entry_probability, 0.0),
  application:set_env(apxr_run, shof_ratio, 1),
  application:set_env(apxr_run, selection_algorithm_efficiency, 1),
  application:set_env(apxr_run, pmp,
    #{data => #{ op_modes => [gt, validation],
                 population_id => {population, testg},
                 polis_id => mathema,
                 survival_percentage => 0.5,
                 init_specie_size => 5,
                 specie_size_limit => 20,
                 generation_limit => 100,
                 evaluations_limit => 5000,
                 fitness_goal => inf
              }}),
  application:set_env(apxr_run, constraints,
    [#{data => #{ morphology => predator,
                 connection_architecture => recurrent,
                 agent_encoding_types => [neural],
                 substrate_plasticities => [l2l_feedfrward, jordan_recurrent, fully_connected],
                 substrate_linkforms => [l2l_feedforward, jordan_recurrent, fully_connected,
                 fully_interconnected, neuronself_recurrent],
                 neural_afs => [tanh, cos, gaussian, absolute, sin, sqrt, sigmoid],
                 neural_pfns => [ojas, hebbian],
                 neural_aggr_fs => [dot_product, mult_product],
                 tuning_selection_fs => [all, all_random, dynamic, dynamic_random,
                 active, active_random, current, current_random],
                 tuning_duration_f => {wsize_proportional, 0.5},
                 annealing_parameters => [0.5, 1.5],
                 perturbation_ranges => [1.0],
                 heredity_types => [darwinian, lamarckian],
                 mutation_operators => [
                          {mutate_weights, 1},
                          {add_bias, 1},
                          {remove_bias, 1},
                          {mutate_af, 1},
                          {add_outlink, 1},
                          {add_inlink, 1},
                          {add_neuron, 1},
                          {outsplice, 1},
                          {add_sensor, 1},
                          {add_sensorlink, 1},
                          {add_actuator, 1},
                          {add_actuatorlink, 1},
                          {mutate_plasticity_parameters, 1}
                 ],
                 tot_topological_mutations_fs => [{ncount_exponential, 0.5},
                 {ncount_exponential, 2.0}],
                 population_evo_alg_f => generational,
                 population_fitness_postprocessor_f => size_proportional,
                 population_selection_f => hof_competition,
                 specie_distinguishers => [tot_n],
                 hof_distinguishers => [tot_n],
                 objectives => [main_fitness, inverse_tot_n]
              }}]),
  application:set_env(apxr_run, runs, 1),

  SpecieCon = lists:last(app_config:get_env(constraints)),
  PopulationId = {population, testgm},
  SpecieId = {specie, 343.62342767},

  Population = models:population(#{
    id => PopulationId,
    specie_ids => [SpecieId],
    morphologies => undefined,
    innovation_factor => undefined,
    evo_alg_f => models:get(population_evo_alg_f, SpecieCon),
    selection_f => models:get(population_selection_f, SpecieCon),
    trace => undefined
  }),
  ok = db:write(Population, population),

  Specie = models:specie(#{
    id => SpecieId,
    population_id => PopulationId,
    fingerprint => origin,
    constraint => SpecieCon,
    fitness => undefined,
    innovation_factor => {0, 0},
    stats => [],
    seed_agent_ids => [],
    hof_distinguishers => [tot_n],
    specie_distinguishers => [tot_n],
    hall_of_fame => []
  }),
  db:write(Specie, specie),

  genotype:construct_agent(SpecieId, ?AGENT_ID, SpecieCon),
  genotype:construct_agent(SpecieId, ?AGENT_ID1, SpecieCon),
  genotype:construct_agent(SpecieId, ?AGENT_ID2, SpecieCon),

  Agent = db:read(?AGENT_ID, agent),
  ACxId = models:get(cx_id, Agent),
  Cx = db:read(ACxId, cortex),
  AIds = models:get(actuator_ids, Cx),
  A3Id = {actuator, {1.0, 33.3336}},

  A3 = models:actuator(#{
    id => A3Id,
    name => {flatland_other_actuator, two_other_wheels},
    type => standard,
    cx_id => ACxId,
    scape => {public, flatland},
    vl => 2,
    fanin_ids => [],
    generation => 1,
    format => no_geo,
    parameters => [2]
  }),
  ok = db:write(A3, actuator),
  UCx = models:set({actuator_ids, [A3Id | AIds]}, Cx),
  db:write(UCx, cortex).

cleanup(_) -> 
  genotype:delete_agent(?AGENT_ID),
  genotype:delete_agent(?AGENT_ID1),
  genotype:delete_agent(?AGENT_ID2),
  db:delete(?N1ID, neuron),
  db:delete(?N2ID, neuron),
  db:delete(?N3ID, neuron),
  db:delete(?N4ID, neuron),
  db:delete(?S1ID, sensor),
  db:delete(?S2ID, sensor),
  db:delete(?A1ID, actuator),
  db:delete(?A2ID, actuator).  