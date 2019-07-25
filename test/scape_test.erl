-module(scape_test).

-include_lib("eunit/include/eunit.hrl").

%% runners

scape_test_() ->
  {setup,
    fun setup/0,
    fun cleanup/1,
  [
    fun scape_subtest/0
  ]}.

%% tests

scape_subtest() ->
  % start
  utils:random_seed(),
  scape:start_link(1.0, 1.0, 50.0, 50.0, flatland),
  AgentIds = [{agent, rand:uniform() - 0.5} || _<- lists:seq(1, 125)],

  % enter
  [test_enter(AId) || AId <- AgentIds],

  timer:sleep(100),

  % sense
  [test_sense(AId) || AId <- AgentIds],
  % actuate
  [test_actuate(AId) || AId <- AgentIds],

  timer:sleep(100),

   % sense
  [test_sense(AId) || AId <- AgentIds],
  % actuate
  [test_actuate(AId) || AId <- AgentIds],

  timer:sleep(100),

   % sense
  [test_sense(AId) || AId <- AgentIds],
  % actuate
  [test_actuate(AId) || AId <- AgentIds],

  timer:sleep(100),

   % sense
  [test_sense(AId) || AId <- AgentIds],
  % actuate
  [test_actuate(AId) || AId <- AgentIds],

  timer:sleep(100),

   % sense
  [test_sense(AId) || AId <- AgentIds],
  % actuate
  [test_actuate(AId) || AId <- AgentIds],

  timer:sleep(100),

  % query_area
  QueryResults = scape:query_area(1.0, 1.0, 50.0, 50.0),
  ?assert(is_list(QueryResults)),

  timer:sleep(100),

  % leave
  [test_leave(AId) || AId <- AgentIds].

%% helpers

test_enter(AId) ->
  Morphologies = [predator, prey],
  Params = [
    {lists:nth(rand:uniform(length(Morphologies)), Morphologies),
    [{sensor, {-1.0, rand:uniform() - 0.5}}],
    [{actuator, {1.0, rand:uniform() - 0.5}}]
  }],
  ?assertEqual(ok, scape:enter(AId, Params)).

test_sense(AId) ->
  SensorPid = self(),
  Params = [],
  ?assertEqual(ok, scape:sense(AId, SensorPid, Params)).

test_actuate(AId) ->
  ActuatorPid = self(),
  Function = two_wheels,
  Params = [(rand:uniform() + 2.0), (rand:uniform() + 2.0)],
  ?assertEqual(ok, scape:actuate(AId, ActuatorPid, Function, Params)).

test_leave(AId) ->
  Params = [],
  ?assertEqual(ok, scape:leave(AId, Params)).

setup() ->
  application:start(shards),

  shards:new(t1, [set, public, named_table, {write_concurrency, true}, {read_concurrency, true}]),
  shards:new(t2, [set, public, named_table, {write_concurrency, true}, {read_concurrency, true}]),
  shards:new(t3, [set, public, named_table, {write_concurrency, true}, {read_concurrency, true}]),

  ets:new(scape_names_pids, [set, public, named_table,
    {write_concurrency, true}, {read_concurrency, true}]),

  {ok, SecPid} = sector_sup:start_link(),

  ets:new(ids_sids_loc, [set, public, named_table,
    {write_concurrency, true}, {read_concurrency, true}]),
  ets:new(xy_pts, [set, public, named_table,
    {write_concurrency, true}, {read_concurrency, true}]),
  ets:new(qt, [set, public, named_table,
    {write_concurrency, true}, {read_concurrency, true}]),

  SecPid.

cleanup(_SecPid) ->
  ok.
  % exit(SecPid, normal),
  % ets:delete(scape_names_pids),
  % ets:delete(ids_sids_loc),
  % ets:delete(xy_pts),
  % ets:delete(qt),
  % application:stop(shards).