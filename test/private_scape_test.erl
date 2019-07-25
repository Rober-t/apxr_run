-module(private_scape_test).

-include_lib("eunit/include/eunit.hrl").

%% macros

-define(AGENT_ID, {agent, 5.92352455}).

%% runners

private_scape_test_() ->
  {setup,
    fun setup/0,
    fun cleanup/1,
  [
    fun private_scape_subtest/0
  ]}.

%% tests

private_scape_subtest() ->
  % setup/start
  meck:expect(test_pscape_mod, init, fun([]) -> {ok, #{}} end),
  {ok, Pid} = private_scape:start_link(?AGENT_ID, test_pscape_mod),
  ?assert(meck:validate(test_pscape_mod)),

  % sense/4
  meck:expect(test_pscape_mod, sense, fun([], #{}) -> {ok, #{}} end),
  ?assertEqual(ok, private_scape:sense(Pid, ?AGENT_ID, self(), [])),
  ?assert(meck:validate(test_pscape_mod)),

  % actuate/5
  meck:expect(test_pscape_mod, actuate, fun(some_fun, [], ?AGENT_ID, #{}) -> {{[1.0], 1}, #{}} end),
  ?assertEqual(ok, private_scape:actuate(Pid, ?AGENT_ID, self(), some_fun, [])),
  ?assert(meck:validate(test_pscape_mod)).

%% helpers

setup() ->
  meck:new(test_pscape_mod, [non_strict]).

cleanup(_) ->
  meck:unload(test_pscape_mod).