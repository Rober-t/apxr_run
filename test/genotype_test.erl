-module(genotype_test).

-include_lib("eunit/include/eunit.hrl").

%% macros

-define(SPECIE_CON, lists:last(app_config:get_env(constraints))).
-define(POPULATION_ID, {population, testg}).
-define(SPECIE_ID, {specie, 55.6767}).
-define(AGENT_ID, {agent, 44.939393}).
-define(AGENT_ID1, {agent, 66.616161}).
-define(AGENT_ID2, {agent, 111.616161}).

%% runners

genotype_test_() ->
  {setup,
    fun setup/0,
    fun cleanup/1,
  [
    fun construct_agent_subtest/0,
    fun update_fingerprint_subtest/0,
    fun clone_agent_neural_encoding_subtest/0,
    fun create_neural_weights_p_subtest/0,
    fun construct_agent_substrate_encoding_subtest/0,
    fun clone_agent_substrate_encoding_subtest/0,
    fun delete_agent_subtest/0
  ]}.

%% tests

construct_agent_subtest() ->
  ?assertEqual(ok, genotype:construct_agent(?SPECIE_ID, ?AGENT_ID, ?SPECIE_CON)),
  ?assertEqual(ok, genotype:construct_agent(?SPECIE_ID, ?AGENT_ID1, ?SPECIE_CON)).

update_fingerprint_subtest() ->
  ?assertEqual(ok, genotype:update_fingerprint(?AGENT_ID)).

clone_agent_neural_encoding_subtest() ->
  {agent, _CAId} = genotype:clone_agent(?AGENT_ID).

create_neural_weights_p_subtest() ->
  [{_, []}, {_, []}] = genotype:create_neural_weights_p(hebbian, 2, []).

construct_agent_substrate_encoding_subtest() ->
  application:set_env(apxr_run, constraints,
    [#{data => #{ morphology => dpb_w_damping,
                 connection_architecture => recurrent,
                 agent_encoding_types => [substrate],
                 substrate_plasticities => [abcn],
                 substrate_linkforms => [l2l_feedforward],
                 neural_afs => [tanh],
                 neural_pfns => [ojas],
                 neural_aggr_fs => [dot_product],
                 tuning_selection_fs => [dynamic_random],
                 tuning_duration_f => {wsize_proportional, 0.5},
                 annealing_parameters => [0.5],
                 perturbation_ranges => [1.0],
                 heredity_types => [darwinian],
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
                 tot_topological_mutations_fs => [{ncount_exponential, 0.5}],
                 population_evo_alg_f => generational,
                 population_fitness_postprocessor_f => size_proportional,
                 population_selection_f => hof_competition,
                 specie_distinguishers => [tot_n],
                 hof_distinguishers => [tot_n],
                 objectives => [main_fitness, inverse_tot_n]
              }}]),
  SpecieCon1 = lists:last(app_config:get_env(constraints)),
  ?assertEqual(ok, genotype:construct_agent(?SPECIE_ID, ?AGENT_ID2, SpecieCon1)).

clone_agent_substrate_encoding_subtest() ->
  {agent, _CAId1} = genotype:clone_agent(?AGENT_ID2).

delete_agent_subtest() ->
  ?assertEqual(ok, genotype:delete_agent(?AGENT_ID)),
  ?assertEqual(ok, genotype:delete_agent(?AGENT_ID1)),
  ?assertEqual(ok, genotype:delete_agent(?AGENT_ID2)).

%% helpers

setup() ->
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
    [#{data => #{ morphology => dpb_w_damping,
                 connection_architecture => recurrent,
                 agent_encoding_types => [neural],
                 substrate_plasticities => [none],
                 substrate_linkforms => [l2l_feedforward],
                 neural_afs => [tanh],
                 neural_pfns => [ojas],
                 neural_aggr_fs => [dot_product],
                 tuning_selection_fs => [dynamic_random],
                 tuning_duration_f => {wsize_proportional, 0.5},
                 annealing_parameters => [0.5],
                 perturbation_ranges => [1.0],
                 heredity_types => [darwinian],
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
                 tot_topological_mutations_fs => [{ncount_exponential, 0.5}],
                 population_evo_alg_f => generational,
                 population_fitness_postprocessor_f => size_proportional,
                 population_selection_f => hof_competition,
                 specie_distinguishers => [tot_n],
                 hof_distinguishers => [tot_n],
                 objectives => [main_fitness, inverse_tot_n]
              }}]),
  application:set_env(apxr_run, runs, 1),

  Population = models:population(#{
    id => ?POPULATION_ID,
    specie_ids => [?SPECIE_ID],
    morphologies => undefined,
    innovation_factor => undefined,
    evo_alg_f => models:get(population_evo_alg_f, ?SPECIE_CON),
    selection_f => models:get(population_selection_f, ?SPECIE_CON),
    trace => undefined
  }),
  ok = db:write(Population, population),

  Specie = models:specie(#{
    id => ?SPECIE_ID,
    population_id => ?POPULATION_ID,
    fingerprint => origin,
    constraint => ?SPECIE_CON,
    fitness => undefined,
    innovation_factor => {0, 0},
    stats => [],
    seed_agent_ids => [],
    hof_distinguishers => [tot_n],
    specie_distinguishers => [tot_n],
    hall_of_fame => []
  }),
  db:write(Specie, specie).

cleanup(_) ->
  db:delete(?POPULATION_ID, population),
  db:delete(?SPECIE_ID, specie).  