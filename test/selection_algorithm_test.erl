-module(selection_algorithm_test).

-include_lib("eunit/include/eunit.hrl").

%% macros

-define(SPECIE_ID, {specie, 0.3535456456}).
-define(AGENT_ID, {agent, 0.65656565756}).
-define(AGENT_ID1, {agent, 0.2342368546}).
-define(CHAMP, build_champ()).

%% runners

selection_algorithm_test_() ->
  {setup,
    fun setup/0,
    fun cleanup/1,
  [
    fun hof_competition_subtest/0,
    fun hof_rank_subtest/0,
    fun hof_top3_subtest/0,
    fun hof_efficiency_subtest/0,
    fun hof_random_subtest/0
  ]}.

%% tests

hof_competition_subtest() ->
  ?assertEqual(ok, selection_algorithm:hof_competition(?SPECIE_ID, [?CHAMP], 2)),
  application:set_env(apxr_run, shof_ratio, 0.5),
  ?assertEqual(ok, selection_algorithm:hof_competition(?SPECIE_ID, [?CHAMP], 2)),
  application:set_env(apxr_run, shof_ratio, 1).

hof_rank_subtest() ->
  ?assertEqual(ok, selection_algorithm:hof_rank(?SPECIE_ID, [?CHAMP], 2)),
  application:set_env(apxr_run, shof_ratio, 0.5),
  ?assertEqual(ok, selection_algorithm:hof_rank(?SPECIE_ID, [?CHAMP], 2)),
  application:set_env(apxr_run, shof_ratio, 1).

hof_top3_subtest() ->
  ?assertEqual(ok, selection_algorithm:hof_top3(?SPECIE_ID, [?CHAMP], 2)),
  application:set_env(apxr_run, shof_ratio, 0.5),
  ?assertEqual(ok, selection_algorithm:hof_top3(?SPECIE_ID, [?CHAMP], 2)),
  application:set_env(apxr_run, shof_ratio, 1).

hof_efficiency_subtest() ->
  ?assertEqual(ok, selection_algorithm:hof_efficiency(?SPECIE_ID, [?CHAMP], 2)),
  application:set_env(apxr_run, shof_ratio, 0.5),
  ?assertEqual(ok, selection_algorithm:hof_efficiency(?SPECIE_ID, [?CHAMP], 2)),
  application:set_env(apxr_run, shof_ratio, 1).

hof_random_subtest() ->
  ?assertEqual(ok, selection_algorithm:hof_random(?SPECIE_ID, [?CHAMP], 2)),
  application:set_env(apxr_run, shof_ratio, 0.5),
  ?assertEqual(ok, selection_algorithm:hof_random(?SPECIE_ID, [?CHAMP], 2)),
  application:set_env(apxr_run, shof_ratio, 1).

%% helpers

setup() ->
  application:set_env(apxr_run, min_pimprovement, 0.0),
  application:set_env(apxr_run, search_params_mut_prob, 0),
  application:set_env(apxr_run, output_sat_limit, 1),
  application:set_env(apxr_run, ro_signal, [0.0]),
  application:set_env(apxr_run, fitness_stagnation, false),
  application:set_env(apxr_run, re_entry_probability, 0.0),
  application:set_env(apxr_run, shof_ratio, 1),
  application:set_env(apxr_run, selection_algorithm_efficiency, 1),
  application:set_env(apxr_run, pmp,
    #{data => #{ op_modes => [gt,  validation],
                 population_id => {population, testsa},
                 polis_id => mathema,
                 survival_percentage => 0.5,
                 init_specie_size => 5,
                 specie_size_limit => 20,
                 generation_limit => 100,
                 evaluations_limit => 5000,
                 fitness_goal => inf
              }}),
  application:set_env(apxr_run, constraints,
    [#{data => #{ morphology => dpb_wo_damping,
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

  SpecieCon = lists:last(app_config:get_env(constraints)),
  PopulationId = {population, testsa},

  ok = genotype:construct_agent(?SPECIE_ID, ?AGENT_ID, SpecieCon),
  ok = genotype:construct_agent(?SPECIE_ID, ?AGENT_ID1, SpecieCon),

  Population = models:population(#{
    id => PopulationId,
    specie_ids => [?SPECIE_ID],
    morphologies => undefined,
    innovation_factor => undefined,
    evo_alg_f => models:get(population_evo_alg_f, SpecieCon),
    selection_f => models:get(population_selection_f, SpecieCon),
    trace => models:trace(#{
      stats => [],
      tot_evaluations => 0,
      step_size => 500
    })
  }),
  ok = db:write(Population, population),

  Specie = models:specie(#{
    id => ?SPECIE_ID,
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
  ok = db:write(Specie, specie),

  Agent = db:read(?AGENT_ID, agent),
  UAgent = models:set([{fitness, 2.4}, {fs, 2.4}, {main_fitness, 2.4}], Agent),
  ok = db:write(UAgent, agent),

  Agent1 = db:read(?AGENT_ID1, agent),
  UAgent1 = models:set([{fitness, 4.4}, {fs, 4.4}, {main_fitness, 4.4}], Agent1),
  ok = db:write(UAgent1, agent),

  Specie = db:read(?SPECIE_ID, specie),
  USpecie = models:set({hall_of_fame, [?CHAMP]}, Specie),
  ok = db:write(USpecie, specie).

cleanup(_) ->
  genotype:delete_agent(?AGENT_ID),
  genotype:delete_agent(?AGENT_ID1).

build_champ() ->
  models:champion(#{
    hof_fingerprint => [1],
    id => ?AGENT_ID1,
    fitness => [2.4],
    validation_fitness => 2.4,
    main_fitness => 2.4,
    tot_n => 1,
    generation => 1,
    fs => 2.4
  }).