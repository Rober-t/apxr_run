-module(substrate_cep_test).

-include_lib("eunit/include/eunit.hrl").

%% runners

substrate_cep_test_() ->
  {setup,
    fun setup/0,
    fun cleanup/1,
  [
    fun substrate_cep_set_weight_subtest/0,
    fun substrate_cep_set_abcn_subtest/0,
    fun substrate_cep_delta_weight_subtest/0
  ]}.

%% tests

substrate_cep_set_weight_subtest() ->
  % start/2
  Pid = substrate_cep:start(node(), exo_pid),

  % init_phase2/8
  ok = substrate_cep:init_phase2(Pid, exo_pid, id, cortex_pid,
    substrate_pid, set_weight, [], [from_pid]),
  
  % forward/3
  meck:expect(substrate, set_weight, fun(substrate_pid, _, [4.1940298507462686]) -> ok end),
  substrate_cep:forward(Pid, from_pid, [3.14]),
  ?assert(meck:validate(substrate)),

  % stop/2
  ?assertEqual(ok, substrate_cep:stop(Pid, exo_pid)).

substrate_cep_set_abcn_subtest() ->
  % start/2
  Pid = substrate_cep:start(node(), exo_pid),

  % init_phase2/8
  ok = substrate_cep:init_phase2(Pid, exo_pid, id, cortex_pid,
    substrate_pid, set_abcn, [], [from_pid]),
  
  % forward/3
  meck:expect(substrate, set_abcn, fun(substrate_pid, _, [3.14]) -> ok end),
  substrate_cep:forward(Pid, from_pid, [3.14]),
  ?assert(meck:validate(substrate)),

  % stop/2
  ?assertEqual(ok, substrate_cep:stop(Pid, exo_pid)).

substrate_cep_delta_weight_subtest() ->
  % start/2
  Pid = substrate_cep:start(node(), exo_pid),

  % init_phase2/8
  ok = substrate_cep:init_phase2(Pid, exo_pid, id, cortex_pid,
    substrate_pid, delta_weight, [], [from_pid]),
  
  % forward/3
  meck:expect(substrate, set_iterative, fun(substrate_pid, _, [4.1940298507462686]) -> ok end),
  substrate_cep:forward(Pid, from_pid, [3.14]),
  ?assert(meck:validate(substrate)),

  % stop/2
  ?assertEqual(ok, substrate_cep:stop(Pid, exo_pid)).

%% helpers

setup() ->
  meck:new(substrate).

cleanup(_) ->
  meck:unload(substrate).