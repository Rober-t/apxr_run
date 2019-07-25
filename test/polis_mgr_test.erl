-module(polis_mgr_test).

-include_lib("eunit/include/eunit.hrl").

%% runners

polis_mgr_test_() ->
  {setup,
    fun setup/0,
    fun cleanup/1,
  [
    fun polis_mgr_subtest_prep/0,
    fun polis_mgr_subtest_setup/0
  ]}.

%% tests

polis_mgr_subtest_prep() ->
  Metadata = #{<<"name">> => <<"foo">>, <<"version">> => <<"1.0.0">>},
  Files = [],
  {ok, {Tarball, _Checksum}} = apxr_tarball:create(Metadata, Files),
  ok = polis_mgr:prep(Tarball).

polis_mgr_subtest_setup() ->
  Config = #{
    exp_parameters => #{
      build_tool => erlang,
      identifier => "local_test",
      public_scape => [],
      runs => 20,
      min_pimprovement => 0.0,
      search_params_mut_prob => 0.5,
      output_sat_limit => 1,
      ro_signal => [0.0],
      fitness_stagnation => false,
      population_mgr_efficiency => 1,
      interactive_selection => false,
      re_entry_probability => 0.0,
      shof_ratio => 1,
      selection_algorithm_efficiency => 1
    },
    pm_parameters => #{
      data => #{
        evaluations_limit => 5000,
        fitness_goal => inf,
        generation_limit => 100,
        init_specie_size => 5,
        op_modes => [gt, validation],
        polis_id => mathema,
        population_id => dtm,
        specie_size_limit => 20,
        survival_percentage => 0.5
      }
    },
    init_constraints => [
      #{
        data => #{
          agent_encoding_types => [substrate],
          annealing_parameters => [0.5],
          connection_architecture => recurrent,
          heredity_types => [darwinian],
          hof_distinguishers => [tot_n],
          morphology => dtm_morphology,
          mutation_operators => [
            [mutate_weights, 1],
            [add_bias, 1],
            [remove_bias, 1],
            [mutate_af, 1],
            [add_outlink, 1],
            [add_inlink, 1],
            [add_neuron, 1],
            [outsplice, 1],
            [add_sensor, 1],
            [add_actuator, 1],
            [add_sensorlink, 1],
            [add_actuatorlink, 1],
            [mutate_plasticity_parameters, 1],
            [add_cpp, 1],
            [add_cep, 1]
          ],
          neural_afs => [tanh, relu],
          neural_aggr_fs => [dot_product, diff_product],
          neural_pfns => [ojas],
          perturbation_ranges => [1],
          population_evo_alg_f => generational,
          population_selection_f => hof_competition,
          specie_distinguishers => [tot_n],
          substrate_linkforms => [l2l_feedforward, jordan_recurrent],
          substrate_plasticities => [abcn, none],
          tot_topological_mutations_fs => [[ncount_exponential, 0.5]],
          tuning_duration_f => [wsize_proportional, 0.5],
          tuning_selection_fs => [dynamic_random]
        }
      }
    ]
  },
  ok = polis_mgr:setup(Config).

%% helpers

setup() ->
  Elixir = code:lib_dir(apxr_run, priv) ++ "/lib/elixir/ebin/",
  code:add_path(Elixir),
  application:start(compiler),
  application:start(elixir),
  polis_mgr:start_link(),
  ok.

cleanup(_) ->
  ok.