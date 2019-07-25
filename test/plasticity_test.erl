-module(plasticity_test).

-include_lib("eunit/include/eunit.hrl").

%% macros

-define(AFS, [tanh, cos, gaussian, absolute, sin, sqrt, sigmoid]).
-define(CXID, {cortex, {origin, 81.678457630318}}).
-define(NHWID, {neuron, {0.0, 1.2131105277401442}}).
-define(NHID, {neuron, {0.0, 2.2719130909152305}}).
-define(NOJWID, {neuron, {0.0, 1.34564564128163}}).
-define(NOJID, {neuron, {0.0, 1.159246038128163}}).
-define(NSMV1ID, {neuron, {0.0, 2.194216939804278}}).
-define(NSMV2ID, {neuron, {0.0, 1.056170344851108}}).
-define(NSMV3ID, {neuron, {0.0, 5.710439851448822}}).
-define(NSMV4ID, {neuron, {0.0, 5.267619846825299}}).
-define(NSMV5ID, {neuron, {0.0, 7.570099812537118}}).
-define(NSMV6ID, {neuron, {0.0, 2.4400325184421328}}).
-define(NNMID, {neuron, {0.0, 2.998667206588721}}).

%% runners

plasticity_test_() ->
  {setup,
    fun setup/0,
    fun cleanup/1,
  [
    fun none_subtest/0,
    fun hebbian_w_subtest/0,
    fun hebbian_subtest/0,
    fun ojas_w_subtest/0,
    fun ojas_subtest/0,
    fun self_modulation_v1_subtest/0,
    fun self_modulation_v2_subtest/0,
    fun self_modulation_v3_subtest/0,
    fun self_modulation_v4_subtest/0,
    fun self_modulation_v5_subtest/0,
    fun self_modulation_v6_subtest/0,
    fun neuromodulation_subtest/0
  ]}.

%% tests

none_subtest() ->
  ?assertEqual([], plasticity:none(neural_parameters)),
  ?assertEqual([], plasticity:none(weight_parameters)),
  ?assertExit("Neuron does not support plasticity.",
    plasticity:none({{neuron, {0.0, test_unique_id()}}, mutate})),

  ?assertEqual([{self(), [{1.2356, [0.5747]}]}],
    plasticity:none([5.0, 0.3423], [{self(), [0.34234]}], [{self(), [{1.2356, [0.5747]}]}], [0.6786])).

hebbian_w_subtest() ->
  NHW = models:neuron(#{
    id => ?NHWID,
    generation => 1,
    cx_id => ?CXID,
    af => lists:nth(rand:uniform(length(?AFS)), ?AFS),
    pf => {hebbian, [rand:uniform() - 0.5]},
    aggr_f => dot_product,
    input_idps => [{{neuron, {0.0, test_unique_id()}},
    [{rand:uniform() - 0.5, [rand:uniform() - 0.5]}]}],
    input_idps_modulation => [],
    output_ids => [],
    ro_ids => []
  }),
  ok = db:write(NHW, neuron),

  ?assertEqual([], plasticity:hebbian_w(neural_parameters)),
  [WPHW] = plasticity:hebbian_w(weight_parameters),
  ?assert(WPHW > -1.0),
  ?assert(WPHW < 1.0),
  ?assertNotEqual(models:get(input_idps, NHW),
    models:get(input_idps, plasticity:hebbian_w({?NHWID, mutate}))),

  ?assertEqual([{self(), [{1.3691096627228, [0.5747]}]}],
  plasticity:hebbian_w([5.0, 0.3423], [{self(), [0.34234]}], [{self(), [{1.2356, [0.5747]}]}], [0.6786])).

hebbian_subtest() ->
  NH = models:neuron(#{
    id => ?NHID,
    generation => 1,
    cx_id => ?CXID,
    af => lists:nth(rand:uniform(length(?AFS)), ?AFS),
    pf => {hebbian, [rand:uniform() - 0.5]},
    aggr_f => dot_product,
    input_idps => [{{neuron, {0.0, test_unique_id()}}, [{rand:uniform() - 0.5, []}]}],
    input_idps_modulation => [],
    output_ids => [],
    ro_ids => []
  }),
  ok = db:write(NH, neuron),

  [HNP] = plasticity:hebbian(neural_parameters),
  ?assert(is_float(HNP), true),
  [] = plasticity:hebbian(weight_parameters),
  ?assertNotEqual(models:get(pf, NH), models:get(pf, plasticity:hebbian({?NHID, mutate}))),

  ?assertEqual([{self(), [{1.3151203715852, []}]}],
    plasticity:hebbian([5.0, 0.3423], [{self(), [0.34234]}], [{self(), [{1.2356, []}]}], [0.6786])).

ojas_w_subtest() ->
  NOJW = models:neuron(#{
    id => ?NOJWID,
    generation => 1,
    cx_id => ?CXID,
    af => lists:nth(rand:uniform(length(?AFS)), ?AFS),
    pf => {hebbian, [rand:uniform() - 0.5]},
    aggr_f => dot_product,
    input_idps => [{{neuron, {0.0, test_unique_id()}},
    [{rand:uniform() - 0.5, [rand:uniform() - 0.5]}]}],
    input_idps_modulation => [],
    output_ids => [],
    ro_ids => []
  }),
  ok = db:write(NOJW, neuron),

  ?assertEqual([], plasticity:ojas_w(neural_parameters)),
  [WPOJW] = plasticity:ojas_w(weight_parameters),
  ?assert(WPOJW > -1.0),
  ?assert(WPOJW < 1.0),
  ?assertNotEqual(models:get(input_idps, NOJW),
    models:get(input_idps, plasticity:ojas_w({?NOJWID, mutate}))),

  ?assertEqual([{self(), [{1.0421103744654128, [0.5747]}]}],
    plasticity:ojas_w([5.0, 0.3423], [{self(), [0.34234]}], [{self(), [{1.2356, [0.5747]}]}], [0.6786])).

ojas_subtest() ->
  NOJ = models:neuron(#{
    id => ?NOJID,
    generation => 1,
    cx_id => ?CXID,
    af => lists:nth(rand:uniform(length(?AFS)), ?AFS),
    pf => {hebbian, [rand:uniform() - 0.5]},
    aggr_f => dot_product,
    input_idps => [{{neuron, {0.0, test_unique_id()}}, [{rand:uniform() - 0.5, []}]}],
    input_idps_modulation => [],
    output_ids => [],
    ro_ids => []
  }),
  ok = db:write(NOJ, neuron),

  [OJNP] = plasticity:ojas(neural_parameters),
  ?assert(is_float(OJNP), true),
  [] = plasticity:ojas(weight_parameters),
  ?assertNotEqual(models:get(pf, NOJ), models:get(pf, plasticity:ojas({?NOJID, mutate}))),

  ?assertEqual([{self(), [{1.1203546566547953, []}]}],
    plasticity:ojas([5.0, 0.3423], [{self(), [0.34234]}], [{self(), [{1.2356, []}]}], [0.6786])).

self_modulation_v1_subtest() ->
  NSMV1 = models:neuron(#{
    id => ?NSMV1ID,
    generation => 1,
    cx_id => ?CXID,
    af => lists:nth(rand:uniform(length(?AFS)), ?AFS),
    pf => {hebbian, [rand:uniform() - 0.5]},
    aggr_f => dot_product,
    input_idps => [{{neuron, {0.0, test_unique_id()}}, [{rand:uniform() - 0.5,
    [rand:uniform() - 0.5]}]}],
    input_idps_modulation => [],
    output_ids => [],
    ro_ids => []
  }),
  ok = db:write(NSMV1, neuron),

  ?assertEqual([0.1, 0, 0, 0], plasticity:self_modulation_v1(neural_parameters)),
  [WPSMV1] = plasticity:self_modulation_v1(weight_parameters),
  ?assert(WPSMV1 > -1.0),
  ?assert(WPSMV1 < 1.0),
  ?assertNotEqual(models:get(input_idps, NSMV1),
    models:get(input_idps, plasticity:self_modulation_v1({?NSMV1ID, mutate}))),

  ?assertEqual([{self(), [{1.2401124966549328, [0.5747]}]}],
  plasticity:self_modulation_v1([5.0, 0.1, 0, 0, 0], [{self(), [0.34234]}],
    [{self(), [{1.2356, [0.5747]}]}], [0.6786])).

self_modulation_v2_subtest() ->
  NSMV2 = models:neuron(#{
    id => ?NSMV2ID,
    generation => 1,
    cx_id => ?CXID,
    af => lists:nth(rand:uniform(length(?AFS)), ?AFS),
    pf => {hebbian, [rand:uniform() - 0.5]},
    aggr_f => dot_product,
    input_idps => [{{neuron, {0.0, test_unique_id()}}, [{rand:uniform() - 0.5,
    [rand:uniform() - 0.5]}]}],
    input_idps_modulation => [],
    output_ids => [],
    ro_ids => []
  }),
  ok = db:write(NSMV2, neuron),

  [_, 0, 0, 0] = plasticity:self_modulation_v2(neural_parameters),
  [WPSMV2] = plasticity:self_modulation_v2(weight_parameters),
  ?assert(WPSMV2 > -1.0),
  ?assert(WPSMV2 < 1.0),
  ?assertNotEqual(models:get(input_idps, NSMV2),
    models:get(input_idps, plasticity:self_modulation_v2({?NSMV2ID, mutate}))),

  ?assertEqual([{self(), [{1.2401124966549328, [0.5747]}]}],
  plasticity:self_modulation_v2([5.0, 0.1, 0, 0, 0], [{self(), [0.34234]}],
    [{self(), [{1.2356, [0.5747]}]}], [0.6786])).

self_modulation_v3_subtest() ->
  NSMV3 = models:neuron(#{
    id => ?NSMV3ID,
    generation => 1,
    cx_id => ?CXID,
    af => lists:nth(rand:uniform(length(?AFS)), ?AFS),
    pf => {hebbian, [rand:uniform() - 0.5]},
    aggr_f => dot_product,
    input_idps => [{{neuron, {0.0, test_unique_id()}}, [{rand:uniform() - 0.5,
    [rand:uniform() - 0.5]}]}],
    input_idps_modulation => [],
    output_ids => [],
    ro_ids => []
  }),
  ok = db:write(NSMV3, neuron),

  [?assert(is_float(NP)) || NP <- [_SMV3NP1, _SMV3NP2, _SMV3NP3, _SMV3NP4]
  = plasticity:self_modulation_v3(neural_parameters)],
  [WPSMV3] = plasticity:self_modulation_v3(weight_parameters),
  ?assert(WPSMV3 > -1.0),
  ?assert(WPSMV3 < 1.0),
  ?assertNotEqual(models:get(input_idps, NSMV3),
    models:get(input_idps, plasticity:self_modulation_v3({?NSMV3ID, mutate}))),

  ?assertEqual([{self(), [{1.2401124966549328, [0.5747]}]}],
  plasticity:self_modulation_v3([5.0, 0.1, 0, 0, 0], [{self(), [0.34234]}],
    [{self(), [{1.2356, [0.5747]}]}], [0.6786])).

self_modulation_v4_subtest() ->
  NSMV4 = models:neuron(#{
    id => ?NSMV4ID,
    generation => 1,
    cx_id => ?CXID,
    af => lists:nth(rand:uniform(length(?AFS)), ?AFS),
    pf => {hebbian, [rand:uniform() - 0.5]},
    aggr_f => dot_product,
    input_idps => [{{neuron, {0.0, test_unique_id()}}, [{rand:uniform() - 0.5,
    [rand:uniform() - 0.5]}]}],
    input_idps_modulation => [],
    output_ids => [],
    ro_ids => []
  }),
  ok = db:write(NSMV4, neuron),

  ?assertEqual([0, 0, 0], plasticity:self_modulation_v4(neural_parameters)),
  [WPSMV4A, WPSMV4B] = plasticity:self_modulation_v4(weight_parameters),
  ?assert(WPSMV4A > -1.0),
  ?assert(WPSMV4B > -1.0),
  ?assert(WPSMV4A < 1.0),
  ?assert(WPSMV4B < 1.0),
  ?assertNotEqual(models:get(input_idps, NSMV4),
    models:get(input_idps, plasticity:self_modulation_v4({?NSMV4ID, mutate}))),

  ?assertEqual([{self(), [{1.2443652091679887, [0.5747, 0.5747]}]}],
  plasticity:self_modulation_v4([5.0, 0, 0, 0], [{self(), [0.34234]}],
    [{self(), [{1.2356, [0.5747, 0.5747]}]}], [0.6786])).

self_modulation_v5_subtest() ->
  NSMV5 = models:neuron(#{
    id => ?NSMV5ID,
    generation => 1,
    cx_id => ?CXID,
    af => lists:nth(rand:uniform(length(?AFS)), ?AFS),
    pf => {hebbian, [rand:uniform() - 0.5]},
    aggr_f => dot_product,
    input_idps => [{{neuron, {0.0, test_unique_id()}}, [{rand:uniform() - 0.5,
    [rand:uniform() - 0.5]}]}],
    input_idps_modulation => [],
    output_ids => [],
    ro_ids => []
  }),
  ok = db:write(NSMV5, neuron),

  [?assert(is_float(NP)) || NP <- [_SMV5NP1, _SMV5NP2, _SMV5NP3]
  = plasticity:self_modulation_v5(neural_parameters)],
  [WPSMV5A, WPSMV5B] = plasticity:self_modulation_v5(weight_parameters),
  ?assert(WPSMV5A > -1.0),
  ?assert(WPSMV5B > -1.0),
  ?assert(WPSMV5A < 1.0),
  ?assert(WPSMV5B < 1.0),
  ?assertNotEqual(models:get(input_idps, NSMV5),
    models:get(input_idps, plasticity:self_modulation_v5({?NSMV5ID, mutate}))),

  ?assertEqual([{self(), [{1.3881727392749774, [0.5747, 0.5747]}]}],
  plasticity:self_modulation_v5([5.0, 0.234, 0.685, 0.1954], [{self(), [0.34234]}],
    [{self(), [{1.2356, [0.5747, 0.5747]}]}], [0.6786])).

self_modulation_v6_subtest() ->
  NSMV6 = models:neuron(#{
    id => ?NSMV6ID,
    generation => 1,
    cx_id => ?CXID,
    af => lists:nth(rand:uniform(length(?AFS)), ?AFS),
    pf => {hebbian, [rand:uniform() - 0.5]},
    aggr_f => dot_product,
    input_idps => [{{neuron, {0.0, test_unique_id()}}, [{rand:uniform() - 0.5,
    [rand:uniform() - 0.5]}]}],
    input_idps_modulation => [],
    output_ids => [],
    ro_ids => []
  }),
  ok = db:write(NSMV6, neuron),

  ?assertEqual([], plasticity:self_modulation_v6(neural_parameters)),
  [WPSMV6A, _WPSMV6B, _WPSMV6C, _WPSMV6D, _WPSMV6E] = plasticity:self_modulation_v6(weight_parameters),
  ?assert(WPSMV6A > -1.0),
  ?assert(WPSMV6A < 1.0),
  ?assertNotEqual(models:get(input_idps, NSMV6),
    models:get(input_idps, plasticity:self_modulation_v6({?NSMV6ID, mutate}))),

  ?assertEqual([{self(), [{1.2930111515582352, [0.5747, 0.5747, 0.234, 0.685, 0.1954]}]}],
  plasticity:self_modulation_v6([5.0], [{self(), [0.34234]}],
    [{self(), [{1.2356, [0.5747, 0.5747, 0.234, 0.685, 0.1954]}]}], [0.6786])).

neuromodulation_subtest() ->
  NNM = models:neuron(#{
    id => ?NNMID,
    generation => 1,
    cx_id => ?CXID,
    af => lists:nth(rand:uniform(length(?AFS)), ?AFS),
    pf => {hebbian, [rand:uniform() - 0.5]},
    aggr_f => dot_product,
    input_idps => [{{neuron, {0.0, test_unique_id()}}, [{rand:uniform() - 0.5,
    [rand:uniform() - 0.5]}]}],
    input_idps_modulation => [],
    output_ids => [],
    ro_ids => []
  }),
  ok = db:write(NNM, neuron),

  [WPNMA, _WPNMB, _WPNMC, _WPNMD, _WPNME] = plasticity:neuromodulation(neural_parameters),
  ?assertEqual([], plasticity:neuromodulation(weight_parameters)),
  ?assert(WPNMA > -1.0),
  ?assert(WPNMA < 1.0),
  ?assertNotEqual(models:get(pf, NNM), models:get(pf, plasticity:neuromodulation({?NNMID, mutate}))),

  ?assertEqual([{self(), [{2.9057581729341253, [0.5747]}]}],
  plasticity:neuromodulation([5.0, 0.234, 0.685, 0.1954, 1.3234, 0.324], [{self(), [0.34234]}],
    [{self(), [{1.2356, [0.5747]}]}], [0.6786])).

%% helpers

setup() ->
  ok.

cleanup(_) ->
  db:delete(?NHWID, neuron),
  db:delete(?NHID, neuron),
  db:delete(?NOJWID, neuron),
  db:delete(?NOJID, neuron),
  db:delete(?NSMV1ID, neuron),
  db:delete(?NSMV2ID, neuron),
  db:delete(?NSMV3ID, neuron),
  db:delete(?NSMV4ID, neuron),
  db:delete(?NSMV5ID, neuron),
  db:delete(?NSMV6ID, neuron),
  db:delete(?NNMID, neuron).

test_unique_id() ->
  (1 / rand:uniform() * 1000000 / 1000000).