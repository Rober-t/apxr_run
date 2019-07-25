-module(agent_mgr_client_test).

-include_lib("eunit/include/eunit.hrl").

%% macros

-define(AGENT_ID, {agent, 5.92352455}).
-define(OP_MODE, gt).

%% runners

agent_mgr_client_test_() ->
  {setup,
    fun setup/0,
    fun cleanup/1,
  [
    fun agent_mgr_client_subtest/0
  ]}.

%% tests

agent_mgr_client_subtest() ->
  % start_agent/2
  meck:expect(agent_mgr, start_agent, fun(?AGENT_ID, ?OP_MODE) -> ok end),
  ?assertEqual(ok, agent_mgr_client:start_agent(?AGENT_ID, ?OP_MODE)),
  ?assert(meck:validate(agent_mgr)),

  % stop_agent/1
  meck:expect(agent_mgr, stop_agent, fun(?AGENT_ID) -> ok end),
  ?assertEqual(ok, agent_mgr_client:stop_agent(?AGENT_ID)),
  ?assert(meck:validate(agent_mgr)).

%% helpers

setup() ->
  application:set_env(apxr_run, build_tool, erlang),
  ets:new(population_status, [set, public, named_table,
    {write_concurrency, true}, {read_concurrency, true}]),
  ets:new(evaluations, [set, public, named_table,
    {write_concurrency, true}, {read_concurrency, true}]),
  ets:new(active_agents, [set, public, named_table,
    {write_concurrency, true}, {read_concurrency, true}]),
  ets:new(inactive_agents, [set, public, named_table,
    {write_concurrency, true}, {read_concurrency, true}]),
  db:start_link(),
  meck:new(agent_mgr).

cleanup(_) ->
  meck:unload(agent_mgr).