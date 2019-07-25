%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Copyright (C) 2018 ApproximateReality
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%----------------------------------------------------------------------------
%%% @doc ApxrRun top level supervisor.
%%% @end
%%%----------------------------------------------------------------------------
-module(apxr_run_sup).

-behaviour(supervisor).

%% Start/Stop
-export([
  start_link/0
]).

%% Supervisor callbacks
-export([
  init/1
]).

%%%============================================================================
%%% Types
%%%============================================================================

-type sup_flags() :: #{
  intensity => non_neg_integer(),
  period => pos_integer(),
  strategy => one_for_all | one_for_one | rest_for_one | simple_one_for_one
}.

-type child_spec() :: [#{
  id := _,
  start := {atom(), atom(), undefined | [any()]},
  modules => dynamic | [atom()],
  restart => permanent | temporary | transient,
  shutdown => brutal_kill | infinity | non_neg_integer(),
  type => supervisor | worker
}].

-export_type([
  sup_flags/0,
  child_spec/0
]).

%%%============================================================================
%%% API
%%%============================================================================

%%-----------------------------------------------------------------------------
%% @doc Starts the supervisor.
%% @end
%%-----------------------------------------------------------------------------
-spec start_link() -> {ok, pid()}.
start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%%============================================================================
%%% Supervisor callbacks
%%%============================================================================

%%-----------------------------------------------------------------------------
%% @private
%% @doc Whenever a supervisor is started using supervisor:start_link,
%%      this function is called by the new process to find out about restart
%%      strategy, maximum restart frequency and child specifications.
%% @end
%%-----------------------------------------------------------------------------
-spec init([]) -> {ok, {sup_flags(), child_spec()}}.
init([]) ->
  ets:new(cowpat, [set, public, named_table,
    {write_concurrency, true}, {read_concurrency, true}]),
  SupFlags = #{
    strategy => one_for_all,
    intensity => 1,
    period => 5
  },
  DB = #{
    id => db,
    start => {db, start_link, []},
    restart => permanent,
    shutdown => 5000,
    type => worker,
    modules => [db]  
  },
  PolisSup = #{
    id => polis_sup,
    start => {polis_sup, start_link, []},
    restart => permanent,
    shutdown => infinity,
    type => supervisor,
    modules => [polis_sup]
  },
  PolisMgr = #{
    id => polis_mgr,
    start => {polis_mgr, start_link, []},
    restart => permanent,
    shutdown => 5000,
    type => worker,
    modules => [polis_mgr]
  },
  ChildSpecs = [DB, PolisSup, PolisMgr],
  {ok, {SupFlags, ChildSpecs}}.

%%%============================================================================
%%% Internal functions
%%%============================================================================