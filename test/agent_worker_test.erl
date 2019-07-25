-module(agent_worker_test).

-include_lib("eunit/include/eunit.hrl").

%% macros

-define(AGENT_ID, {agent, 1.334}).
-define(OP_MODE, gt).

%% runners

agent_worker_test_() ->
  {setup,
    fun setup/0,
    fun cleanup/1,
  [
    fun agent_worker_subtest/0
  ]}.

%% tests

agent_worker_subtest() ->
  meck:expect(exoself, start, fun(_) -> self() end),
  meck:expect(exoself, init_phase2, fun(_, ?AGENT_ID, ?OP_MODE) -> ok end),
  {ok, _} = agent_worker:start_link(?AGENT_ID, ?OP_MODE),
  ?assert(meck:validate(exoself)).

%% helpers

setup() ->
  ets:new(agent_ids_pids, [set, public, named_table,
    {write_concurrency, true}, {read_concurrency, true}]),
  meck:new(exoself).

cleanup(_) ->
  %ets:delete(agent_ids_pids),
  meck:unload(exoself).