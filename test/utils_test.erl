-module(utils_test).

-include_lib("eunit/include/eunit.hrl").

%% runners

utils_test_() ->
  {setup,
    fun setup/0,
    fun cleanup/1,
  [
    fun vec1_dominates_vec2_subtest/0,
    fun get_module_subtest/0,
    fun system_metrics_subtest/0,
    fun safe_serialize_erlang_subtest/0,
    fun safe_binary_to_term_subtest/0,
    fun random_seed_subtest/0
  ]}.

%% tests

vec1_dominates_vec2_subtest() ->
  ?assertEqual(true, utils:vec1_dominates_vec2([2.435435, 3.54643], [1.23590, 0.342], 0.0)),
  ?assertEqual(false, utils:vec1_dominates_vec2([0.435435, 1.2342], [1.23590, 4.234], 0.0)),
  ?assertEqual(false, utils:vec1_dominates_vec2([0.435435, 23.3242], [1.23590, 12.342], 0.0)),
  ?assertEqual(true, utils:vec1_dominates_vec2([4.435435, 3.54643], [1.23590, 0.342], 2.0)),
  ?assertEqual(false, utils:vec1_dominates_vec2([2.435435, 3.54643], [1.23590, 0.342], 2.0)),
  ?assertEqual(false, utils:vec1_dominates_vec2([0.435435, 1.2342], [1.23590, 4.234], 2.0)),
  ?assertEqual(false, utils:vec1_dominates_vec2([0.435435, 23.3242], [1.23590, 12.342], 2.0)).

get_module_subtest() ->
  ?assertEqual(test_module, utils:get_module(test_module)),
  application:set_env(apxr_run, build_tool, elixir),
  ?assertEqual('Elixir.TestModule', utils:get_module(test_module)).

system_metrics_subtest() ->
  #{memory := _MemeroyUsage, scheduler_usage := _SchedulerUsage} = utils:system_metrics().

safe_serialize_erlang_subtest() ->
  ?assertEqual(
    <<131,108,0,0,0,5,108,0,0,0,2,109,0,0,0,4,116,101,115,116,109,
      0,0,0,4,116,101,115,116,106,108,0,0,0,2,109,0,0,0,4,115,113,
      100,102,97,123,106,107,0,3,115,100,102,109,0,0,0,4,113,115,
      100,102,70,63,201,153,153,153,153,153,154,106>>,
    utils:safe_serialize_erlang([{test, test}, {sqdf, 123}, "sdf", qsdf, 0.2])).

safe_binary_to_term_subtest() ->
  Bin = term_to_binary([{test, test}]),
  ?assertEqual({ok, [{test, test}]}, utils:safe_binary_to_term(Bin)).

random_seed_subtest() ->
  {#{bits := 64, jump := _, next := _, type := exs1024s, weak_low_bits := _}, _} =
  utils:random_seed().

%% helpers

setup() ->
  application:set_env(apxr_run, build_tool, erlang),
  application:start(compiler),
  application:start(elixir),
  ok.

cleanup(_) ->
  application:set_env(apxr_run, build_tool, erlang),
  ok.