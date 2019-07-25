-module(tot_topological_mutations_test).

-include_lib("eunit/include/eunit.hrl").

%% macros

-define(CXID, {cortex, {origin, 38.7370377260318}}).
-define(AGENT_ID, {neuron, {0.0, 0.656565}}).

%% runners

tot_topological_mutations_test_() ->
  {setup,
    fun setup/0,
    fun cleanup/1,
  [
    fun ncount_exponential_subtest/0,
    fun ncount_linear_subtest/0
  ]}.

%% tests

ncount_exponential_subtest() ->
  ?assert(tot_topological_mutations:ncount_exponential(1.0, ?AGENT_ID) =< 5),
  ?assert(tot_topological_mutations:ncount_exponential(1.0, ?AGENT_ID) =< 25).

ncount_linear_subtest() ->
  ?assertEqual(20, tot_topological_mutations:ncount_linear(4, ?AGENT_ID)).

%% helpers

setup() ->
  SpecieId = {specie, 0.3535},

  NIds = [
    {neuron,{0.0,9.657190484886447}},
    {neuron,{0.0,2.8348400212276608}},
    {neuron,{0.0,1.7546606231346502}},
    {neuron,{0.0,1.0664152660075847}},
    {neuron,{0.0,2.2524777677148906}}
  ],

  SpecC = #{data => #{
               morphology => dtm_morphology,
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
               heredity_types => [lamarckian],
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
            }},

  Cortex = models:cortex(#{
    id => ?CXID,
    agent_id => ?AGENT_ID,
    neuron_ids => NIds,
    sensor_ids => [],
    actuator_ids => []
  }),

  Agent = models:agent(#{
    id => ?AGENT_ID,
    encoding_type => neural,
    generation => 1,
    population_id => {population, test},
    specie_id => SpecieId,
    cx_id => ?CXID,
    fingerprint => undefined,
    constraint => SpecC,
    evo_hist => [],
    fitness => 0.0,
    innovation_factor => 0,
    pattern => [],
    tuning_selection_f => [dynamic_random],
    annealing_parameter => [0.5],
    tuning_duration_f => {wsize_proportional, 0.5},
    perturbation_range => [1.0],
    perturbation_qty => multiple,
    mutation_operators => [{mutate_weights, 1}, {add_bias, 1}, {remove_bias, 1}],
    tot_topological_mutations_f => {ncount_exponential, 0.5},
    heredity_type => darwinian,
    substrate_id => {substrate, {void, 0.0}},
    offspring_ids => [],
    parent_ids => [],
    champion_flag => false,
    behavioral_trace => false,
    fs => 1.0,
    main_fitness => undefined
  }),

  ok = db:write(Agent, agent),
  ok = db:write(Cortex, cortex).

cleanup(_) ->
  db:delete(?AGENT_ID, agent),
  db:delete(?CXID, cortex).