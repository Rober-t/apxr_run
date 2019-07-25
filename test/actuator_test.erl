-module(actuator_test).

-include_lib("eunit/include/eunit.hrl").

%% macros

-define(AGENT_ID, {agent, 345.55}).
-define(ACTUATOR_ID, {actuator, 45.355}).

%% runners

actuator_test_() ->
  {setup,
    fun setup/0,
    fun cleanup/1,
  [
    fun actuator_subtest/0
  ]}.

%% tests

actuator_subtest() ->
  % start/2
  Pid = actuator:start(node(), exo_pid),

  % init_phase2/11
  meck:expect(test_actuator_mod, init, fun([]) -> {ok, #{}} end),
  ok = actuator:init_phase2(Pid, exo_pid, ?ACTUATOR_ID, ?AGENT_ID,
    cortex_pid, scape_pid, {test_actuator_mod, some_actuator}, 1, [], [from_pid], gt),
  ?assert(meck:validate(test_actuator_mod)),
  
  % forward
  meck:expect(test_actuator_mod, actuate, fun(some_actuator,
    {?AGENT_ID, [3.14], [], 1, scape_pid, ?ACTUATOR_ID, #{}}) -> #{} end),
  Pid ! {forward, from_pid, [3.14]},
  ?assert(meck:validate(test_actuator_mod)),
  
  % fitness/2
  meck:expect(cortex, sync, fun(cortex_pid, _, [3.34], 2) -> ok end),
  ok = actuator:fitness(Pid, {[3.34], 2}),
  ?assert(meck:validate(cortex)),
  
  % stop/2
  ?assertEqual(ok, actuator:stop(Pid, exo_pid)).

%% helpers

setup() ->
  application:set_env(apxr_run, build_tool, erlang),
  meck:new(cortex),
  meck:new(test_actuator_mod, [non_strict]).

cleanup(_) ->
  meck:unload(cortex),
  meck:unload(test_actuator_mod).