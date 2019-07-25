-module(cortex_test).

-include_lib("eunit/include/eunit.hrl").

%% runners

cortex_test_() ->
  {setup,
    fun setup/0,
    fun cleanup/1,
  [
    fun cortex_subtest/0
  ]}.

%% tests

cortex_subtest() ->
  % start/2
  Pid = cortex:start(node(), exo_pid),

  % init_phase2/7
  meck:expect(sensor, sync, fun(spid, _) -> ok end),
  ok = cortex:init_phase2(Pid, exo_pid, id, [spid], [npid], [apid], gt),
  ?assert(meck:validate(sensor)),
  
  % sync/4
  ok = cortex:sync(Pid, apid, [3.14], 0),
  meck:expect(exoself, evaluation_complete, fun(exo_pid, _, [6.28], 2, _, false) -> ok end),
  ok = cortex:sync(Pid, apid, [3.14], 1),
  ?assert(meck:validate(exoself)),
  
  % reactivate/2
  meck:expect(sensor, sync, fun(spid, _) -> ok end),
  ok = cortex:init_phase2(Pid, exo_pid, id, [spid], [npid], [apid], gt),
  ?assert(meck:validate(sensor)),

  % stop/2
  ?assertEqual(ok, cortex:stop(Pid, exo_pid)).

%% helpers

setup() ->
  meck:new(sensor),
  meck:new(exoself, [non_strict]).

cleanup(_) ->
  meck:unload(sensor),
  meck:unload(exoself).