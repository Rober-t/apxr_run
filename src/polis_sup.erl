%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Copyright (C) 2018 ApproximateReality
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%----------------------------------------------------------------------------
%%% @doc Polis supervisor.
%%% @end
%%%----------------------------------------------------------------------------
-module(polis_sup).

-behaviour(supervisor).

%% Start/Stop
-export([
  start_link/0
]).

%% Supervisor callbacks
-export([
  init/1
]).

%% Xref
-ignore_xref([
  start_link/0
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
  SupFlags = #{
    strategy => one_for_all,
    intensity => 2,
    period => 10
  },
  ScapeMgrSup = #{
    id => scape_mgr_sup,
    start => {scape_mgr_sup, start_link, []},
    restart => permanent,
    shutdown => infinity,
    type => supervisor,
    modules => [scape_mgr_sup]
  },
  ExperimentMgrSup = #{
    id => experiment_mgr_sup,
    start => {experiment_mgr_sup, start_link, []},
    restart => permanent,
    shutdown => infinity,
    type => supervisor,
    modules => [experiment_mgr_sup]
  },
  PopulationMgrSup = #{
    id => population_mgr_sup,
    start => {population_mgr_sup, start_link, []},
    restart => permanent,
    shutdown => infinity,
    type => supervisor,
    modules => [population_mgr_sup]
  },
  AgentMgrSup = #{
    id => agent_mgr_sup,
    start => {agent_mgr_sup, start_link, []},
    restart => permanent,
    shutdown => infinity,
    type => supervisor,
    modules => [experiment_mgr_sup]
  },
  ChildSpecs = [ScapeMgrSup, ExperimentMgrSup, PopulationMgrSup, AgentMgrSup],
  {ok, {SupFlags, ChildSpecs}}.

%%%============================================================================
%%% Internal functions
%%%============================================================================