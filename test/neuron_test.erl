-module(neuron_test).

-include_lib("eunit/include/eunit.hrl").

%% runners

neuron_test_() ->
  {setup,
    fun setup/0,
    fun cleanup/1,
  [
    fun neuron_subtest/0
  ]}.

%% tests

neuron_subtest() ->
  Id = {neuron, {0.0, 1.0017706671595699}},
  HeredityType = darwinian,
  AggrF = dot_product,
  Af = tanh,
  Pf = {ojas_w, [0.6137726485964126]},
  SIPidPs = [{self(), [{0.25, [0.5]}]}],
  MIPidPs = [{self(), [{0.25, [0.5]}]}],
  OutputPids = [self()],
  ROPids = [],

  % start/2
  Pid = neuron:start(node(), exo_pid),

  % init_phase2/12
  ok = neuron:init_phase2(Pid, exo_pid, Id, cortex_pid, Af, Pf, AggrF,
    HeredityType, SIPidPs, MIPidPs, OutputPids, ROPids),
  
  % forward/3
  ok = neuron:forward(Pid, self(), [88.88]),
  ok = neuron:forward(Pid, self(), [88.88]),

  % weight_backup/2
  ok = neuron:weight_backup(Pid, exo_pid),

  % weight_restore/2
  ok = neuron:weight_restore(Pid, exo_pid),

  % weight_perturb/3
  ok = neuron:weight_perturb(Pid, exo_pid, 4),

  % perturb_pf/2
  {ojas_w, [Val]} = neuron:perturb_pf(4.0, {ojas_w, [0.6]}),
  ?assertEqual(true, is_float(Val)),

  % perturb_weights_p/4
  [5.9, {Valp, [0.6]}] = neuron:perturb_weights_p(4.0, 4.0, [{2.324, [0.6]}], [5.9]),
  ?assertEqual(true, is_float(Valp)),

  % stop/2
  ?assertEqual(ok, neuron:stop(Pid, exo_pid)).

%% helpers

setup() ->
  ok.

cleanup(_) ->
  ok.