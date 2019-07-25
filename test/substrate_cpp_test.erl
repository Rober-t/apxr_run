-module(substrate_cpp_test).

-include_lib("eunit/include/eunit.hrl").

%% runners

substrate_cpp_test_() ->
  {setup,
    fun setup/0,
    fun cleanup/1,
  [
    fun substrate_cpp_subtest/0,
    fun substrate_cpp_iow_subtest/0
  ]}.

%% tests

substrate_cpp_subtest() ->
  % start/2
  Pid = substrate_cpp:start(node(), exo_pid),

  % init_phase2/9
  ok = substrate_cpp:init_phase2(Pid, exo_pid, id, cortex_pid, substrate_pid, cartesian, 1, [], [from_pid]),

  % neurode_coordinates/4
  meck:expect(neuron, forward, fun(from_pid, _, [3.14, 6.28]) -> ok end),
  substrate_cpp:neurode_coordinates(Pid, substrate_pid, [3.14], [6.28]),
  ?assert(meck:validate(neuron)),

  % stop/2
  ?assertEqual(ok, substrate_cpp:stop(Pid, exo_pid)).

substrate_cpp_iow_subtest() ->
  % start/2
  Pid = substrate_cpp:start(node(), exo_pid),

  % init_phase2/9
  ok = substrate_cpp:init_phase2(Pid, exo_pid, id, cortex_pid, substrate_pid, cartesian, 1, [], [from_pid]),

  % neurode_coordinates_iow/5
  meck:expect(neuron, forward, fun(from_pid, _,  [1.3, 2.4, 1.3, 3.14, 6.28]) -> ok end),
  substrate_cpp:neurode_coordinates_iow(Pid, substrate_pid, [3.14], [6.28], [1.3, 2.4, 1.3]),
  ?assert(meck:validate(neuron)),

  % stop/2
  ?assertEqual(ok, substrate_cpp:stop(Pid, exo_pid)).

%% helpers

setup() ->
  meck:new(neuron).

cleanup(_) ->
  meck:unload(neuron).