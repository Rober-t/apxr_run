-module(scape_mgr_client_test).

-include_lib("eunit/include/eunit.hrl").

%% macros

-define(AGENT_ID, {agent, 5.92352455}).

%% runners

scape_mgr_client_test_() ->
  {setup,
    fun setup/0,
    fun cleanup/1,
  [
    fun scape_mgr_client_subtest/0
  ]}.

%% tests

scape_mgr_client_subtest() ->
  % start_scape/5
  meck:expect(scape_mgr, start_scape, fun(1.0, 1.0, 1.0, 1.0, testmod) -> ok end),
  ?assertEqual(ok, scape_mgr_client:start_scape(1.0, 1.0, 1.0, 1.0, testmod)),
  ?assert(meck:validate(scape_mgr)),

  % stop_scape/1
  meck:expect(scape_mgr, stop_scape, fun(testmod) -> ok end),
  ?assertEqual(ok, scape_mgr_client:stop_scape(testmod)),
  ?assert(meck:validate(scape_mgr)),

  % enter/2
  meck:expect(scape, enter, fun(?AGENT_ID, []) -> ok end),
  ?assertEqual(ok, scape_mgr_client:enter(?AGENT_ID, [])),
  ?assert(meck:validate(scape)),

  % sense/3
  meck:expect(scape, sense, fun(?AGENT_ID, pid, []) -> ok end),
  ?assertEqual(ok, scape_mgr_client:sense(?AGENT_ID, pid, [])),
  ?assert(meck:validate(scape)),

  % actuate/4
  meck:expect(scape, actuate, fun(?AGENT_ID, pid, some_fun, []) -> ok end),
  ?assertEqual(ok, scape_mgr_client:actuate(?AGENT_ID, pid, some_fun, [])),
  ?assert(meck:validate(scape)),

  % leave/2
  meck:expect(scape, leave, fun(?AGENT_ID, []) -> ok end),
  ?assertEqual(ok, scape_mgr_client:leave(?AGENT_ID, [])),
  ?assert(meck:validate(scape)).

%% helpers

setup() ->
  meck:new(scape_mgr),
  meck:new(scape).

cleanup(_) ->
  meck:unload(scape_mgr),
  meck:unload(scape).