-module(population_mgr_client_test).

-include_lib("eunit/include/eunit.hrl").

%% macros

-define(AGENT_ID, {agent, 5.92352455}).
-define(SPECIE_ID, {specie, 5.92352455}).

%% runners

population_mgr_client_test_() ->
  {setup,
    fun setup/0,
    fun cleanup/1,
  [
    fun population_mgr_client_subtest/0
  ]}.

%% tests

population_mgr_client_subtest() ->
  % new_run/0
  meck:expect(population_mgr_sup, start_population_mgr, fun() -> ok end),
  ?assertEqual(ok, population_mgr_client:new_run()),
  ?assert(meck:validate(population_mgr_sup)),

  % restart_run/0
  meck:expect(population_mgr_sup, restart_population_mgr, fun() -> ok end),
  ?assertEqual(ok, population_mgr_client:restart_run()),
  ?assert(meck:validate(population_mgr_sup)),

  % agent_terminated/1
  meck:expect(population_mgr, agent_terminated, fun(?AGENT_ID) -> ok end),
  ?assertEqual(ok, population_mgr_client:agent_terminated(?AGENT_ID)),
  ?assert(meck:validate(population_mgr)),

  % set_goal_reached/1
  meck:expect(population_mgr, set_goal_reached, fun() -> ok end),
  ?assertEqual(ok, population_mgr_client:set_goal_reached()),
  ?assert(meck:validate(population_mgr)),

  % set_evaluations/1
  meck:expect(population_mgr, set_evaluations, fun(?SPECIE_ID, 1, 1, 1) -> ok end),
  ?assertEqual(ok, population_mgr_client:set_evaluations(?SPECIE_ID, 1, 1, 1)),
  ?assert(meck:validate(population_mgr)),

  % validation_complete/1
  meck:expect(population_mgr, validation_complete, fun(?AGENT_ID, 1.0) -> ok end),
  ?assertEqual(ok, population_mgr_client:validation_complete(?AGENT_ID, 1.0)),
  ?assert(meck:validate(population_mgr)).

%% helpers

setup() ->
  meck:new(population_mgr),
  meck:new(population_mgr_sup).

cleanup(_) ->
  meck:unload(population_mgr),
  meck:unload(population_mgr_sup).