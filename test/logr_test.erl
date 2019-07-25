-module(logr_test).

-include_lib("eunit/include/eunit.hrl").

%% runners

logr_test_() ->
  {setup,
    fun setup/0,
  [
    fun logr_subtest/0,
    fun logr_scape_filter_subtest/0,
    fun logr_status_filter_subtest/0
  ]}.

%% tests

logr_subtest() ->
  % debug/1
  ?assertEqual(ok, logr:debug({debug_component, testing, ok, "testing details",
    ["test", "parameters", 1234]})),

  % info/1
  ?assertEqual(ok, logr:info({info_component, testing, ok, "testing details",
    ["test", "parameters", 1234]})),

  % notice/1
  ?assertEqual(ok, logr:notice({notice_component, testing, ok, "testing details",
    ["test", "parameters", 1234]})),

  % warning/1
  ?assertEqual(ok, logr:warning({warning_component, testing, ok, "testing details",
    ["test", "parameters", 1234]})),

  % error/1
  ?assertEqual(ok, logr:error({error_component, testing, ok, "testing details",
    ["test", "parameters", 1234]})),

  % critical/1
  ?assertEqual(ok, logr:critical({critical_component, testing, ok, "testing details",
    ["test", "parameters", 1234]})),

  % alert/1
  ?assertEqual(ok, logr:alert({alert_component, testing, ok, "testing details",
    ["test", "parameters", 1234]})),

  % emergency/1
  ?assertEqual(ok, logr:emergency({emergency_component, testing, ok, "testing details",
    ["test", "parameters", 1234]})).

logr_scape_filter_subtest() ->
  Log1 = #{in => info_component, what => testing, result => ok, details => "testing details",
    params => list_to_tuple(["test", "parameters", 1234])},
  LogEvent1 = #{level => info, msg => {report, Log1}, meta => #{}},
  ?assertEqual(ignore, logr:scape_filter(LogEvent1, log)),
  
  Log2 = #{in => scape, what => testing, result => ok, details => "testing details",
    params => list_to_tuple(["test", "parameters", 1234])},
  LogEvent2 = #{level => info, msg => {report, Log2}, meta => #{}},
  ?assertEqual(LogEvent2, logr:scape_filter(LogEvent2, log)).

logr_status_filter_subtest() ->
  Log1 = #{in => info_component, what => testing, result => ok, details => "testing details",
    params => list_to_tuple(["test", "parameters", 1234])},
  LogEvent1 = #{level => info, msg => {report, Log1}, meta => #{}},
  ?assertEqual(ignore, logr:status_filter(LogEvent1, log)),
  
  Log2 = #{in => info_component, what => status, result => ok, details => "testing details",
    params => list_to_tuple(["test", "parameters", 1234])},
  LogEvent2 = #{level => info, msg => {report, Log2}, meta => #{}},
  ?assertEqual(LogEvent2, logr:status_filter(LogEvent2, log)),

  Log3 = #{in => info_component, what => testing, result => error, details => "testing details",
    params => list_to_tuple(["test", "parameters", 1234])},
  LogEvent3 = #{level => info, msg => {report, Log3}, meta => #{}},
  ?assertEqual(LogEvent3, logr:status_filter(LogEvent3, log)).

%% helpers

setup() ->
  ok.