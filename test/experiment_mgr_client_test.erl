-module(experiment_mgr_client_test).

-include_lib("eunit/include/eunit.hrl").

%% runners

experiment_mgr_client_test_() ->
  {setup,
    fun setup/0,
    fun cleanup/1,
  [
    fun experiment_mgr_client_subtest/0
  ]}.

%% tests

experiment_mgr_client_subtest() ->
  % start_run/0
  meck:expect(experiment_mgr, run, fun() -> ok end),
  ?assertEqual(ok, experiment_mgr_client:start_run()),
  ?assert(meck:validate(experiment_mgr)),

  % run_complete/2
  meck:expect(experiment_mgr, complete, fun(population_id, []) -> ok end),
  ?assertEqual(ok, experiment_mgr_client:run_complete(population_id, [])),
  ?assert(meck:validate(experiment_mgr)).

%% helpers

setup() ->
  meck:new(experiment_mgr).

cleanup(_) ->
  meck:unload(experiment_mgr).