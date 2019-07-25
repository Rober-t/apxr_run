-module(population_stats_test).

-include_lib("eunit/include/eunit.hrl").

%% macros

-define(POPULATION_ID, {population, testps}).
-define(SPECIE_ID, {specie, 0.6767}).
-define(AGENT_ID, {agent, 0.939392343}).
-define(AGENT_ID1, {agent, 0.61634161}).

%% runners

population_stats_test_() ->
  {setup,
    fun setup/0,
    fun cleanup/1,
  [
    fun calculate_specie_fitness_subtest/0,
    fun calculate_specie_avg_nodes_subtest/0,
    fun calculate_specie_diversity_subtest/0
  ]}.

%% tests

calculate_specie_fitness_subtest() ->
  ?assertEqual(
    {[3.4000000000000004], [1.0000000000000002], [4.4], [2.4]},
    population_mgr:calculate_specie_fitness(?SPECIE_ID)
  ).

calculate_specie_avg_nodes_subtest() ->
  ?assertEqual({1.0, 0.0}, population_mgr:calculate_specie_avg_nodes(?SPECIE_ID)).

calculate_specie_diversity_subtest() ->
  ?assertEqual(1, population_mgr:calculate_specie_diversity(?SPECIE_ID)).

%% helpers

setup() ->
  % ets:new(population_status, [set, public, named_table,
  %   {write_concurrency, true}, {read_concurrency, true}]),
  % ets:new(evaluations, [set, public, named_table,
  %   {write_concurrency, true}, {read_concurrency, true}]),
  % ets:new(active_agents, [set, public, named_table,
  %   {write_concurrency, true}, {read_concurrency, true}]),
  % ets:new(inactive_agents, [set, public, named_table,
  %   {write_concurrency, true}, {read_concurrency, true}]),
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
                 population_id => {population, testps},
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

  SpecieCon = lists:last(app_config:get_env(constraints)),

  Stat = models:stat(#{
          morphology => dpb_w_damping,
          specie_id => ?SPECIE_ID,
          avg_neurons => 1.0,
          std_neurons => 1.0,
          avg_fitness => [2.3523],
          std_fitness => [2.324],
          max_fitness => [4.324],
          min_fitness => [1.324],
          avg_diversity => 1,
          evaluations => 10,
          time_stamp => 999999,
          validation_fitness => {0.0, void}
        }),

  Trace = models:trace(#{
    stats => [[Stat]],
    tot_evaluations => 10,
    step_size => 400
  }),

  Population = models:population(#{
    id => ?POPULATION_ID,
    specie_ids => [?SPECIE_ID],
    morphologies => undefined,
    innovation_factor => undefined,
    evo_alg_f => models:get(population_evo_alg_f, SpecieCon),
    selection_f => models:get(population_selection_f, SpecieCon),
    trace => Trace
  }),
  ok = db:write(Population, population),

  Specie = models:specie(#{
    id => ?SPECIE_ID,
    population_id => ?POPULATION_ID,
    fingerprint => origin,
    constraint => SpecieCon,
    fitness => undefined,
    innovation_factor => {0, 0},
    stats => [[Stat]],
    seed_agent_ids => [],
    hof_distinguishers => [tot_n],
    specie_distinguishers => [tot_n],
    hall_of_fame => []
  }),
  db:write(Specie, specie),

  genotype:construct_agent(?SPECIE_ID, ?AGENT_ID, SpecieCon),
  genotype:construct_agent(?SPECIE_ID, ?AGENT_ID1, SpecieCon),

  {agent, UAId} = ?AGENT_ID,
  {agent, UAId1} = ?AGENT_ID1,
  ets:insert(active_agents, {UAId, agent, ?SPECIE_ID}),
  ets:insert(active_agents, {UAId1, agent, ?SPECIE_ID}),

  Agent = db:read(?AGENT_ID, agent),
  UAgent = models:set([{fitness, [2.4]}, {fs, 2.5}, {main_fitness, 2.6}], Agent),
  db:write(UAgent, agent),

  Agent1 = db:read(?AGENT_ID1, agent),
  UAgent1 = models:set([{fitness, [4.4]}, {fs, 4.4}, {main_fitness, 4.4}], Agent1),
  db:write(UAgent1, agent),

  Champ = models:champion(#{
    hof_fingerprint => [1],
    id => ?AGENT_ID1,
    fitness => [2.7],
    validation_fitness => 2.8,
    main_fitness => 2.9,
    tot_n => 1,
    generation => 1,
    fs => 3.0
  }),

  Specie = db:read(?SPECIE_ID, specie),
  USpecie = models:set({hall_of_fame, [Champ]}, Specie),
  db:write(USpecie, specie),
  ets:insert(evaluations, {?SPECIE_ID, 32}),
  meck:new(experiment_mgr).

cleanup(_) ->
  genotype:delete_agent(?AGENT_ID),
  genotype:delete_agent(?AGENT_ID1),
  meck:unload(experiment_mgr).
