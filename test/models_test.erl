-module(models_test).

-include_lib("eunit/include/eunit.hrl").

%% macros

-define(CXID, {cortex, {origin, 38.7370377260318}}).
-define(NID, {neuron, {0.0, 1.0882294868042244}}).
-define(NID1, {neuron, {0.0, 1.2651305755444655}}).
-define(N, build_n()).
-define(N1, build_n1()).

%% runners

models_test_() ->
  {setup,
    fun setup/0,
    fun cleanup/1,
  [
    fun get_subtest/0,
    fun set_subtest/0,
    fun read_subtest/0,
    fun write_subtest/0,
    fun delete_subtest/0
  ]}.

%% tests

get_subtest() ->
  ?assertEqual(dot_product, models:get(aggr_f, ?N)),
  ?assertEqual([?NID, ?CXID], models:get([id, cx_id], ?N)).

set_subtest() ->
  UN = models:set({aggr_f, mult_product}, ?N),
  ?assertEqual(mult_product, models:get(aggr_f, UN)),
  UN1 = models:set([{generation, 2}, {pf, {ojas, [0.234]}}], UN),
  ?assertEqual([2, {ojas, [0.234]}], models:get([generation, pf], UN1)).

read_subtest() ->
  ?assertEqual(?N, db:read(?NID, neuron)).

write_subtest() ->
  ?assertEqual(?N1, db:read(?NID1, neuron)).

delete_subtest() ->
  ?assertEqual(?N1, db:read(?NID1, neuron)),
  ok = db:delete(?NID1, neuron),
  ?assertEqual(not_found, db:read(?NID1, neuron)).

%% helpers

setup() ->
  ok = db:write(?N, neuron),
  ok = db:write(?N1, neuron).

build_n() ->
  models:neuron(#{
    id => ?NID,
    generation => 1,
    cx_id => ?CXID,
    af => tanh,
    pf => {hebbian, [0.03453245345]},
    aggr_f => dot_product,
    input_idps => [{{neuron,{0.0,1.0146107682820593}},[{0.05581502457792553,[]}]}],
    input_idps_modulation => [],
    output_ids => [],
    ro_ids => []
  }).

build_n1() ->
  models:neuron(#{
    id => ?NID1,
    generation => 1,
    cx_id => ?CXID,
    af => gaussian,
    pf => {hebbian, [0.05326992332221325]},
    aggr_f => dot_product,
    input_idps => [{{neuron,{0.0,1.0146107682820593}},[{0.05581502457792553,[]}]}],
    input_idps_modulation => [],
    output_ids => [],
    ro_ids => []
  }).

cleanup(_) ->
  db:delete(?NID, neuron).