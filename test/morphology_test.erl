-module(morphology_test).

-include_lib("eunit/include/eunit.hrl").

%% runners

morphology_test_() ->
  [
    fun get_init_sensors_subtest/0,
    fun get_init_actuators_subtest/0,
    fun get_sensors_subtest/0,
    fun get_actuators_subtest/0,
    fun get_init_substrate_cpps_subtest/0,
    fun get_init_substrate_ceps_subtest/0,
    fun get_substrate_cpps_subtest/0,
    fun get_substrate_ceps_subtest/0
  ].

%% tests

get_init_sensors_subtest() ->
  ?assertEqual([#{data =>
                      #{cx_id => undefined, fanout_ids => [],
                        format => no_geo, generation => undefined,
                        id => undefined,
                        name => {flatland_sensor, distance_scanner},
                        parameters => [[1.5707963267948966], [5], [0.0]],
                        scape => {public, flatland},
                        type => standard, vl => 5}}],
    morphology:get_init_sensors(prey)).

get_init_actuators_subtest() ->
  ?assertEqual([#{data =>
                      #{cx_id => undefined, fanin_ids => [], format => no_geo,
                        generation => undefined, id => undefined,
                        name => {flatland_actuator, two_wheels},
                        parameters => [2],
                        scape => {public, flatland},
                        type => standard, vl => 2}}],
    morphology:get_init_actuators(predator)).

get_sensors_subtest() ->
  ?assertEqual([#{data =>
                      #{cx_id => undefined, fanout_ids => [],
                        format => no_geo, generation => undefined,
                        id => undefined,
                        name => {flatland_sensor, distance_scanner},
                        parameters => [[1.5707963267948966], [5], [0.0]],
                        scape => {public, flatland},
                        type => standard, vl => 5}},
                #{data =>
                      #{cx_id => undefined, fanout_ids => [],
                        format => no_geo, generation => undefined,
                        id => undefined,
                        name => {flatland_sensor, color_scanner},
                        parameters => [[1.5707963267948966], [5], [0.0]],
                        scape => {public, flatland},
                        type => standard, vl => 5}}],
    morphology:get_sensors(predator)).

get_actuators_subtest() ->
  ?assertEqual([#{data =>
                      #{cx_id => undefined, fanin_ids => [], format => no_geo,
                        generation => undefined, id => undefined,
                        name => {flatland_actuator, two_wheels},
                        parameters => [2],
                        scape => {public, flatland},
                        type => standard, vl => 2}}],
    morphology:get_actuators(prey)).

get_init_substrate_cpps_subtest() ->
  ?assertEqual([#{data =>
                      #{cx_id => undefined, fanout_ids => [],
                        format => undefined, generation => undefined,
                        id => undefined, name => cartesian,
                        parameters => undefined, scape => undefined,
                        type => substrate, vl => 4}}],
    morphology:get_init_substrate_cpps(2, none)).

get_init_substrate_ceps_subtest() ->
  ?assertEqual([#{data =>
                      #{cx_id => undefined, fanin_ids => [],
                        format => undefined, generation => undefined,
                        id => undefined, name => set_weight,
                        parameters => undefined, scape => undefined,
                        type => substrate, vl => 1}}],
    morphology:get_init_substrate_ceps(4, none)).

get_substrate_cpps_subtest() ->
  ?assertEqual([#{data =>
                      #{cx_id => undefined, fanout_ids => [],
                        format => undefined, generation => undefined,
                        id => undefined, name => cartesian,
                        parameters => undefined, scape => undefined,
                        type => substrate, vl => 9}},
                #{data =>
                      #{cx_id => undefined, fanout_ids => [],
                        format => undefined, generation => undefined,
                        id => undefined, name => centripital_distances,
                        parameters => undefined, scape => undefined,
                        type => substrate, vl => 5}},
                #{data =>
                      #{cx_id => undefined, fanout_ids => [],
                        format => undefined, generation => undefined,
                        id => undefined, name => cartesian_distance,
                        parameters => undefined, scape => undefined,
                        type => substrate, vl => 4}},
                #{data =>
                      #{cx_id => undefined, fanout_ids => [],
                        format => undefined, generation => undefined,
                        id => undefined, name => cartesian_coord_diffs,
                        parameters => undefined, scape => undefined,
                        type => substrate, vl => 6}},
                #{data =>
                      #{cx_id => undefined, fanout_ids => [],
                        format => undefined, generation => undefined,
                        id => undefined,
                        name => cartesian_gaussed_coord_diffs,
                        parameters => undefined, scape => undefined,
                        type => substrate, vl => 6}},
                #{data =>
                      #{cx_id => undefined, fanout_ids => [],
                        format => undefined, generation => undefined,
                        id => undefined, name => iow, parameters => undefined,
                        scape => undefined, type => substrate, vl => 3}},
                #{data =>
                      #{cx_id => undefined, fanout_ids => [],
                        format => undefined, generation => undefined,
                        id => undefined, name => spherical,
                        parameters => undefined, scape => undefined,
                        type => substrate, vl => 9}}],
    morphology:get_substrate_cpps(3, iterative)),
    ?assertEqual([#{data =>
                      #{cx_id => undefined, fanout_ids => [],
                        format => undefined, generation => undefined,
                        id => undefined, name => cartesian,
                        parameters => undefined, scape => undefined,
                        type => substrate, vl => 4}},
                #{data =>
                      #{cx_id => undefined, fanout_ids => [],
                        format => undefined, generation => undefined,
                        id => undefined, name => centripital_distances,
                        parameters => undefined, scape => undefined,
                        type => substrate, vl => 2}},
                #{data =>
                      #{cx_id => undefined, fanout_ids => [],
                        format => undefined, generation => undefined,
                        id => undefined, name => cartesian_distance,
                        parameters => undefined, scape => undefined,
                        type => substrate, vl => 1}},
                #{data =>
                      #{cx_id => undefined, fanout_ids => [],
                        format => undefined, generation => undefined,
                        id => undefined, name => cartesian_coord_diffs,
                        parameters => undefined, scape => undefined,
                        type => substrate, vl => 2}},
                #{data =>
                      #{cx_id => undefined, fanout_ids => [],
                        format => undefined, generation => undefined,
                        id => undefined,
                        name => cartesian_gaussed_coord_diffs,
                        parameters => undefined, scape => undefined,
                        type => substrate, vl => 2}},
                #{data =>
                      #{cx_id => undefined, fanout_ids => [],
                        format => undefined, generation => undefined,
                        id => undefined, name => polar,
                        parameters => undefined, scape => undefined,
                        type => substrate, vl => 4}}],
    morphology:get_substrate_cpps(2, none)).

get_substrate_ceps_subtest() ->
  ?assertEqual([#{data =>
                      #{cx_id => undefined, fanin_ids => [],
                        format => undefined, generation => undefined,
                        id => undefined, name => set_abcn,
                        parameters => undefined, scape => undefined,
                        type => substrate, vl => 5}}],
    morphology:get_substrate_ceps(2, abcn)).