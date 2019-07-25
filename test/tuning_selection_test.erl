-module(tuning_selection_test).

-include_lib("eunit/include/eunit.hrl").

%% macros

-define(CXID, {cortex, {origin, 38.7370377260318}}).
-define(NID2, {neuron, {0.0, 5.585330747505288}}).
-define(NIDS, [
  {neuron,{0.0,9.657190484886447}},
  {neuron,{0.0,2.8348400212276608}},
  {neuron,{0.0,1.7546606231346502}},
  {neuron,{0.0,1.0664152660075847}},
  {neuron,{0.0,2.2524777677148906}}
]).

%% runners

tuning_selection_test_() ->
  {setup,
    fun setup/0,
    fun cleanup/1,
  [
    fun dynamic_subtest/0,
    fun dynamic_random_subtest/0,
    fun active_subtest/0,
    fun active_random_subtest/0,
    fun current_subtest/0,
    fun current_random_subtest/0,
    fun all_subtest/0,
    fun all_random_subtest/0
  ]}.

%% tests

dynamic_subtest() ->
  ?assertEqual(
    {lists:nth(1, lists:reverse(?NIDS)), 6.283185307179586},
    lists:nth(1, tuning_selection:dynamic(?NIDS, 0, 1.0, 0.5))
  ),
  ?assertEqual(
    {lists:nth(1, lists:reverse(?NIDS)), 3.141592653589793},
    lists:nth(1, tuning_selection:dynamic(?NIDS, 0, 1.0, 1.0))
  ),
  ?assertEqual(5 > length(tuning_selection:dynamic(?NIDS, 5, 1.0, 0.5)), true).

dynamic_random_subtest() ->
  {NIdDR, 6.283185307179586} = lists:nth(1, tuning_selection:dynamic_random(?NIDS, 0, 1.0, 0.5)),
  ?assertEqual(lists:member(NIdDR, ?NIDS), true).

active_subtest() ->
  ?assertEqual(
    {lists:nth(1, lists:reverse(?NIDS)), 6.283185307179586},
    lists:nth(1, tuning_selection:active(?NIDS, 0, 1.0, 0.5))
  ),
  ?assertEqual([], tuning_selection:active(?NIDS, 5, 1.0, 0.5)).

active_random_subtest() ->
  {NIdAR, 6.283185307179586} = lists:nth(1, tuning_selection:active_random(?NIDS, 0, 1.0, 0.5)),
  ?assertEqual(lists:member(NIdAR, ?NIDS), true).

current_subtest() ->
  ?assertEqual(5, length(tuning_selection:current(?NIDS, 0, 1.0, 0.5))),

  db:write(models:neuron(#{
    id => ?NID2,
    generation => 10,
    cx_id => ?CXID,
    af => tan,
    pf => {none, []},
    aggr_f => dot_product,
    input_idps => [],
    input_idps_modulation => [],
    output_ids => [],
    ro_ids => []
  }), neuron),
  ?assertEqual(lists:member({?NID2, 6.283185307179586},
    tuning_selection:current([?NID2 | ?NIDS], 0, 1.0, 0.5)), false),

  NId3 = {neuron, {0.0, 3.7023488799926048}},
  db:write(models:neuron(#{
    id => NId3,
    generation => 1,
    cx_id => ?CXID,
    af => cos,
    pf => {none, []},
    aggr_f => dot_product,
    input_idps => [],
    input_idps_modulation => [],
    output_ids => [],
    ro_ids => []
  }), neuron),
  ?assertEqual(lists:member({NId3, 6.283185307179586},
    tuning_selection:current([NId3 | ?NIDS], 0, 1.0, 0.5)), true).

current_random_subtest() ->
  {NIdCR, 6.283185307179586} = lists:nth(1, tuning_selection:current_random(?NIDS, 0, 1.0, 0.5)),
  ?assertEqual(lists:member(NIdCR, ?NIDS), true).

all_subtest() ->
  ?assertEqual(5 == length(tuning_selection:all(?NIDS, 0, 1.0, 0.5)), true),
  ?assertEqual(6 == length(tuning_selection:all([?NID2 | ?NIDS], 0, 1.0, 0.5)), true).

all_random_subtest() ->
  {NIdALLR, 6.283185307179586} = lists:nth(1, tuning_selection:all_random(?NIDS, 0, 1.0, 0.5)),
  ?assertEqual(lists:member(NIdALLR, ?NIDS), true).

setup() ->
  [db:write(models:neuron(#{
      id => NId,
      generation => 1,
      cx_id => ?CXID,
      af => sigmoid,
      pf => {none, []},
      aggr_f => dot_product,
      input_idps => [],
      input_idps_modulation => [],
      output_ids => [],
      ro_ids => []
    }), neuron) || NId <- ?NIDS].

cleanup(_) ->
  [db:delete(NId, neuron) || NId <- ?NIDS].