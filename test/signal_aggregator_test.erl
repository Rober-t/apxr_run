-module(signal_aggregator_test).

-include_lib("eunit/include/eunit.hrl").

%% macros

-define(PIDS, [pid1, pid2, pid3, pid4, pid5]).
-define(IACC, iacc()).
-define(IACC2, iacc2()).
-define(IPIDPS, ipidps()).

%% runners

signal_aggregator_test_() ->
  [
    fun dot_product_subtest/0,
    fun diff_product_subtest/0,
    fun mult_product_subtest/0
  ].

%% tests

dot_product_subtest() ->
  ?assertEqual(5.0347878, signal_aggregator:dot_product(?IACC, ?IPIDPS)).

diff_product_subtest()->
  ?assertEqual(5.0347878, signal_aggregator:diff_product(?IACC, ?IPIDPS)),
  ?assertEqual(281.22, signal_aggregator:diff_product(?IACC2, ?IPIDPS)).

mult_product_subtest() ->
  ?assertEqual(0.001006196127860842, signal_aggregator:mult_product(?IACC, ?IPIDPS)).

%% helpers

iacc() ->
  [{Pid, [2.34234, 0.345]} || Pid <- ?PIDS].

iacc2() ->
  [{Pid, [123.34234, 21.345]} || Pid <- ?PIDS].

ipidps() ->
  [{Pid, [{0.234, 0.5}, {1.33, 0.5}]} || Pid <- ?PIDS].