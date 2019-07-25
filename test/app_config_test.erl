-module(app_config_test).

-include_lib("eunit/include/eunit.hrl").

%% runners

app_config_test_() ->
  {setup,
    fun setup/0,
  [
    fun app_config_subtest/0
  ]}.

%% tests

app_config_subtest() ->
  % get_env/1
  ?assertEqual(testing1, app_config:get_env(test)),

  % get_env/2
  ?assertEqual(testing2, app_config:get_env(other_space, test_two)),

  % get_all/0
  ?assertEqual(lists:sort([{test_other, testing3}, {test_two, testing2}]),
	lists:sort(app_config:get_all(other_space))).

%% helpers

setup() ->
  application:set_env(apxr_run, jwt_apxr_io, "test_token"),
  application:set_env(apxr_run, test, testing1),
  application:set_env(other_space, test_two, testing2),
  application:set_env(other_space, test_other, testing3),
  application:set_env(apxr_run, test_other, testing4).