-module(tuning_duration_test).

-include_lib("eunit/include/eunit.hrl").

%% macros

-define(CXID, {cortex, {origin, 38.73324260318}}).
-define(NID1, {neuron, {0.0, 1.4947780822482732}}).
-define(NID2, {neuron, {0.0, 2.0703147894593346}}).
-define(NID3, {neuron, {0.0, 1.3476029283449762}}).
-define(NID4, {neuron, {0.0, 2.3654460499739934}}).

%% runners

tuning_duration_test_() ->
  {setup,
    fun setup/0,
    fun cleanup/1,
  [
    fun const_subtest/0,
    fun wsize_proportional_subtest/0,
    fun nsize_proportional_subtest/0
  ]}.

%% tests

const_subtest() ->
  ?assertEqual(3, tuning_duration:const(3, [?NID1, ?NID2, ?NID3, ?NID4], 1)).

wsize_proportional_subtest() ->
  ?assertEqual(10, tuning_duration:wsize_proportional(3, [?NID1, ?NID2, ?NID3, ?NID4], 1)).

nsize_proportional_subtest() ->
  ?assertEqual(120, tuning_duration:nsize_proportional(3, [?NID1, ?NID2, ?NID3, ?NID4], 1)).

%% helpers

setup() ->
  AFs = [tanh, cos, gaussian, absolute, sin, sqrt, sigmoid],

  NIds = [
    {neuron,{0.0,9.6571947357886447}},
    {neuron,{0.0,2.83484473572276608}},
    {neuron,{0.0,1.75466473571346502}},
    {neuron,{0.0,1.06641473570075847}},
    {neuron,{0.0,2.25247473577148906}}
  ],

  InputIdPs = [{NId, [{rand:uniform() - 0.5, [rand:uniform() - 0.5]}]} || NId <- NIds],

  N1 = models:neuron(#{
    id => ?NID1,
    generation => 1,
    cx_id => ?CXID,
    af => lists:nth(rand:uniform(length(AFs)), AFs),
    pf => {hebbian, [rand:uniform() - 0.5]},
    aggr_f => dot_product,
    input_idps => InputIdPs,
    input_idps_modulation => [],
    output_ids => [],
    ro_ids => []
  }),
  N2 = models:neuron(#{
    id => ?NID2,
    generation => 1,
    cx_id => ?CXID,
    af => lists:nth(rand:uniform(length(AFs)), AFs),
    pf => {hebbian, [rand:uniform() - 0.5]},
    aggr_f => dot_product,
    input_idps => InputIdPs,
    input_idps_modulation => [],
    output_ids => [],
    ro_ids => []
  }),
  N3 = models:neuron(#{
    id => ?NID3,
    generation => 1,
    cx_id => ?CXID,
    af => lists:nth(rand:uniform(length(AFs)), AFs),
    pf => {hebbian, [rand:uniform() - 0.5]},
    aggr_f => dot_product,
    input_idps => InputIdPs,
    input_idps_modulation => [],
    output_ids => [],
    ro_ids => []
  }),
  N4 = models:neuron(#{
    id => ?NID4,
    generation => 1,
    cx_id => ?CXID,
    af => lists:nth(rand:uniform(length(AFs)), AFs),
    pf => {hebbian, [rand:uniform() - 0.5]},
    aggr_f => dot_product,
    input_idps => InputIdPs,
    input_idps_modulation => [],
    output_ids => [],
    ro_ids => []
  }),

  ok = db:write(N1, neuron),
  ok = db:write(N2, neuron),
  ok = db:write(N3, neuron),
  ok = db:write(N4, neuron).

cleanup(_) ->
  db:delete(?NID1, neuron),
  db:delete(?NID2, neuron),
  db:delete(?NID3, neuron),
  db:delete(?NID4, neuron).