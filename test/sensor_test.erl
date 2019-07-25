-module(sensor_test).

-include_lib("eunit/include/eunit.hrl").

%% macros

-define(AGENT_ID, {agent, 345.55}).
-define(SENSOR_ID, {sensor, 45.355}).

%% runners

sensor_test_() ->
  {setup,
    fun setup/0,
    fun cleanup/1,
  [
    fun sensor_subtest/0
  ]}.

%% tests

sensor_subtest() ->
  % start/2
  Pid = sensor:start(node(), exo_pid),

  % init_phase2/11
  meck:expect(test_sensor_mod, init, fun([]) -> {ok, #{}} end),
  ok = sensor:init_phase2(Pid, exo_pid, ?SENSOR_ID, ?AGENT_ID,
    cortex_pid, scape_pid, {test_sensor_mod, some_sensor}, 1, [], [], gt),
  ?assert(meck:validate(test_sensor_mod)),
  
  % sync/2
  meck:expect(test_sensor_mod, sense, fun(some_sensor,
    {?AGENT_ID, 1, [], scape_pid, ?SENSOR_ID, gt, #{}}) -> ok end),
  ok = sensor:sync(Pid, cortex_pid),
  ?assert(meck:validate(test_sensor_mod)),

  % percept/2
  meck:expect(test_sensor_mod, percept, fun(some_sensor,
    {[9.8], ?AGENT_ID, 1, [], #{}}) -> {[2.3], #{}} end),
  ?assertEqual(ok, sensor:percept(Pid, [9.8])),
  ?assert(meck:validate(test_sensor_mod)),

  % stop/2
  ?assertEqual(ok, sensor:stop(Pid, exo_pid)).

%% helpers

setup() ->
  meck:new(test_sensor_mod, [non_strict]).

cleanup(_) ->
  meck:unload(test_sensor_mod).