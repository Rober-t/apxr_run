-module(functions_test).

-include_lib("eunit/include/eunit.hrl").

%% runners

functions_test_() ->
  [
    fun saturation_subtest/0,
    fun scale_subtest/0,
    fun sat_subtest/0,
    fun sat_dzone_subtest/0,
    fun tanh_subtest/0,
    fun relu_subtest/0,
    fun cos_subtest/0,
    fun sin_subtest/0,
    fun sgn_subtest/0,
    fun bin_subtest/0,
    fun trinary_subtest/0,
    fun multiquadric_subtest/0,
    fun absolute_subtest/0,
    fun linear_subtest/0,
    fun quadratic_subtest/0,
    fun gaussian_subtest/0,
    fun sqrt_subtest/0,
    fun log_subtest/0,
    fun sigmoid_subtest/0,
    fun sigmoid1_subtest/0,
    fun avg_subtest/0,
    fun std_subtest/0,
    fun cartesian_subtest/0,
    fun polar_subtest/0,
    fun spherical_subtest/0,
    fun centripital_distances_subtest/0,
    fun cartesian_distance_subtest/0,
    fun cartesian_coord_diffs_subtest/0,
    fun cartesian_gaussed_coord_diffs_subtest/0,
    fun iow_subtest/0,
    fun to_cartesian_subtest/0,
    fun normalize_subtest/0,
    fun spherical2cartesian_subtest/0,
    fun cartesian2spherical_subtest/0,
    fun polar2cartesian_subtest/0,
    fun cartesian2polar_subtest/0,
    fun distance_subtest/0,
    fun vector_difference_subtest/0
  ].

%% tests

saturation_subtest() ->
  ?assertEqual(1000.0, functions:saturation(1500.0)),
  ?assertEqual(-1000.0, functions:saturation(-1500.0)),
  ?assertEqual(-500.0, functions:saturation(-500.0)),

  ?assertEqual(500.0, functions:saturation(1500.0, 500.0)),
  ?assertEqual(-500.0, functions:saturation(-1500.0, 500.0)),
  ?assertEqual(-250.0, functions:saturation(-250.0), 500.0).

scale_subtest() ->
  ?assertEqual([6.583333333333335, 0.9166666666666665], functions:scale([12.3, 5.5], 5.6, 3.2)).

sat_subtest() ->
  ?assertEqual(750.0, functions:sat(1500.0, 500.0, 750.0)),
  ?assertEqual(500.0, functions:sat(-1500.0, 500.0, 750.0)),
  ?assertEqual(500.0, functions:sat(-250.0, 500.0, 750.00)).

sat_dzone_subtest() ->
  ?assertEqual(0.0, functions:sat_dzone(550.0, 250.0, 750.0, 600.0, 500.0)),
  ?assertEqual(750.0, functions:sat_dzone(1500.0, 250.0, 750.0, 500.0, 600.0)).

tanh_subtest() ->
  ?assertEqual(0.9950547536867305, functions:tanh(3.0)).

relu_subtest() ->
  ?assertEqual(0.0, functions:relu(-3.0)),
  ?assertEqual(3.0, functions:relu(3.0)).

cos_subtest() ->
  ?assertEqual(-0.9899924966004454, functions:cos(3.0)).

sin_subtest() ->
  ?assertEqual(0.1411200080598672, functions:sin(3.0)).

sgn_subtest() ->
  ?assertEqual(0, functions:sgn(0)),
  ?assertEqual(1, functions:sgn(3)),
  ?assertEqual(-1, functions:sgn(-5)).

bin_subtest() ->
  ?assertEqual(0, functions:bin(0)),
  ?assertEqual(1, functions:bin(3)),
  ?assertEqual(0, functions:bin(-5)).

trinary_subtest() ->
  ?assertEqual(0, functions:trinary(0.11)),
  ?assertEqual(1, functions:trinary(44.3)),
  ?assertEqual(-1, functions:trinary(-5.44)).

multiquadric_subtest() ->
  ?assertEqual(0.26, functions:multiquadric(0.24)).

absolute_subtest() ->
  ?assertEqual(23.24, functions:absolute(-23.24)).

linear_subtest() ->
  ?assertEqual(3.24, functions:linear(3.24)).

quadratic_subtest() ->
  ?assertEqual(175.29760000000002, functions:quadratic(13.24)).

gaussian_subtest() ->
  ?assertEqual(3.7200757651350987e-44, functions:gaussian(13.24)),
  ?assertEqual(3.7200757651350987e-44, functions:gaussian(130.24)),
  ?assertEqual(2.7602616032381225e-5, functions:gaussian(3.24)),

  ?assertEqual(7.888609052210118e-31, functions:gaussian(2, 13.24)),
  ?assertEqual(7.888609052210118e-31, functions:gaussian(2, 130.24)),
  ?assertEqual(6.91683662039504e-4, functions:gaussian(2, 3.24)).

sqrt_subtest() ->
  ?assertEqual(2.0, functions:sqrt(4.0)).

log_subtest() ->
  ?assertEqual(0.0, functions:log(0.0)),
  ?assertEqual(2.302585092994046, functions:log(10.0)),
  ?assertEqual(4.605170185988092, functions:log(100.0)),
  ?assertEqual(9.210340371976184, functions:log(10000.0)).

sigmoid_subtest() ->
  ?assertEqual(0.9999546021312976, functions:sigmoid(15.0)),
  ?assertEqual(0.9999546021312976, functions:sigmoid(150.0)),
  ?assertEqual(0.5307419243727364, functions:sigmoid(0.123123)),
  ?assertEqual(4.5397868702434395e-5, functions:sigmoid(-12.0)),
  ?assertEqual(4.5397868702434395e-5, functions:sigmoid(-120.0)).

sigmoid1_subtest() ->
  ?assertEqual(0.9375, functions:sigmoid1(15.0)),
  ?assertEqual(0.10962557084130588, functions:sigmoid1(0.123123)),
  ?assertEqual(-0.9230769230769231, functions:sigmoid1(-12.0)).

avg_subtest() ->
  ?assertEqual(4.0, functions:avg([1.0, 3.0, 5.0, 7.0])).

std_subtest() ->
  functions:std([1.0, 3.0, 5.0, 7.0]).

cartesian_subtest() ->
  ?assertEqual([1.0, 3.0, 5.0, 2.0], functions:cartesian([1.0, 3.0], [5.0, 2.0])),
  ?assertEqual(
    [0.234, 0.435, 0.452, 1.0, 3.0, 5.0, 2.0],
    functions:cartesian([1.0, 3.0], [5.0, 2.0], [0.234, 0.435, 0.452])
  ).

polar_subtest() ->
  ?assertEqual([3.1622776601683795, 0.3217505543966422,
    5.385164807134504,  1.1902899496825317],
    functions:polar([1.0, 3.0], [5.0, 2.0])),
  ?assertEqual(
    [0.234, 0.435, 0.452, 3.1622776601683795, 0.3217505543966422,
    5.385164807134504, 1.1902899496825317],
    functions:polar([1.0, 3.0], [5.0, 2.0], [0.234, 0.435, 0.452])
  ).

spherical_subtest() ->
  ?assertEqual([3.9102429592034302, 0.9167136023805363, 1.312184710298928,
    7.697402159170326, 0.348771003583907, 0.8637778515112974],
    functions:spherical([1.0, 3.0, 2.3], [5.0, 2.0, 5.5])),
  ?assertEqual(
    [0.234, 0.435, 0.452, 6.34428877022476, 0.49934672168013006, 1.4125141593135133,
    6.368673331236264, 0.5317240672588056, 0.6679502792822516],
    functions:spherical([1.0, 3.0, 5.5], [5.0, 2.0, 3.4], [0.234, 0.435, 0.452])
  ).

centripital_distances_subtest() ->
  ?assertEqual([3.1622776601683795, 5.385164807134504],
    functions:centripital_distances([1.0, 3.0], [5.0, 2.0])),
  ?assertEqual(
    [0.234, 0.435, 0.452, 3.1622776601683795, 5.385164807134504],
    functions:centripital_distances([1.0, 3.0], [5.0, 2.0], [0.234, 0.435, 0.452])
  ).

cartesian_distance_subtest() ->
  ?assertEqual([4.123105625617661], functions:cartesian_distance([1.0, 3.0], [5.0, 2.0])),
  ?assertEqual(
    [0.234, 0.435, 0.452, 4.123105625617661],
    functions:cartesian_distance([1.0, 3.0], [5.0, 2.0], [0.234, 0.435, 0.452])
  ).

cartesian_coord_diffs_subtest() ->
  ?assertEqual([4.0, -1.0], functions:cartesian_coord_diffs([1.0, 3.0], [5.0, 2.0])),
  ?assertEqual(
    [0.234, 0.435, 0.452, 4.0, -1.0],
    functions:cartesian_coord_diffs([1.0, 3.0], [5.0, 2.0], [0.234, 0.435, 0.452])
  ).

cartesian_gaussed_coord_diffs_subtest() ->
  ?assertEqual([1.1253517369854603e-7, 0.36787944096289676],
    functions:cartesian_gaussed_coord_diffs([1.0, 3.0], [5.0, 2.0])),
  ?assertEqual(
    [0.234, 0.435, 0.452, 1.1253517369854603e-7, 0.36787944096289676],
    functions:cartesian_gaussed_coord_diffs([1.0, 3.0], [5.0, 2.0], [0.234, 0.435, 0.452])
  ).

iow_subtest() ->
  ?assertEqual(
    [0.234, 0.435, 0.452],
    functions:iow([1.0, 3.0], [5.0, 2.0], [0.234, 0.435, 0.452])
  ).

to_cartesian_subtest() ->
  ?assertEqual({cartesian, {2.34, 1.234234}},
    functions:to_cartesian({cartesian, {2.34, 1.234234}})),
  ?assertEqual({cartesian, {1.2876346542142487, 0.30724758082457304, 0.0}},
    functions:to_cartesian({polar, {1.323784, 0.234234}})),
  ?assertEqual({cartesian, {0.41054289273812966, 0.09796125803672603, 1.2546960603277875}},
    functions:to_cartesian({spherical, {1.323784, 0.234234, 0.3245}})).

normalize_subtest() ->
  ?assertEqual([0.16012815380508713, 0.48038446141526137,
    0.8006407690254357, 0.32025630761017426], functions:normalize([1.0, 3.0, 5.0, 2.0])).

spherical2cartesian_subtest() ->
  ?assertEqual({-0.9001976297355174, 0.12832006020245673, -0.4161468365471424},
    functions:spherical2cartesian({1.0, 3.0, 2.0})).

cartesian2spherical_subtest() ->
  ?assertEqual({3.1622776601683795, 1.2490457723982544, 1.5707963267948966},
    functions:cartesian2spherical({1.0, 3.0})),
  ?assertEqual({3.7416573867739413, 1.2490457723982544, 1.0068536854342678},
    functions:cartesian2spherical({1.0, 3.0, 2.0})).

polar2cartesian_subtest() ->
  ?assertEqual({-2.080734182735712, 4.546487134128409, 0.0}, functions:polar2cartesian({5.0, 2.0})).

cartesian2polar_subtest() ->
  ?assertEqual({5.385164807134504, 0.3805063771123649}, functions:cartesian2polar({5.0, 2.0})),
  ?assertEqual({5.385164807134504, 0.3805063771123649}, functions:cartesian2polar({5.0, 2.0, 34.34})).

distance_subtest() ->
  ?assertEqual(5.277336923108093, functions:distance([1.0, 3.0, 5.0], [0.234, 0.435, 0.452])),
  ?assertEqual(6.322474594650421, functions:distance([1.0, 3.0, 5.0], [0.234, 0.435, 0.452], 12.1234)).

vector_difference_subtest() ->
  ?assertEqual([-0.766, -2.565, -4.548],
    functions:vector_difference([1.0, 3.0, 5.0], [0.234, 0.435, 0.452])),
  ?assertEqual([3.4, 2.0, 5.0, -0.766, -2.565, -4.548],
    functions:vector_difference([1.0, 3.0, 5.0], [0.234, 0.435, 0.452], [5.0, 2.0, 3.4])).