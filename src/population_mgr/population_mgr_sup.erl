%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Copyright (C) 2018 ApproximateReality
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%----------------------------------------------------------------------------
%%% @doc PopulationMgr top supervisor.
%%% @end
%%%----------------------------------------------------------------------------
-module(population_mgr_sup).

-behaviour(supervisor).

%% Start/Stop
-export([
  start_link/0
]).

%% API
-export([
  start_population_mgr/0,
  restart_population_mgr/0
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
%%% Type
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

%%-----------------------------------------------------------------------------
%% @doc Spawns populaton_monitor.
%% @end
%%-----------------------------------------------------------------------------
-spec start_population_mgr() -> {ok, pid()}.
start_population_mgr() ->
  PopulationMgr = #{
    id => population_mgr_worker,
    start => {population_mgr_worker, start_link, []},
    restart => transient,
    shutdown => 30000,
    type => worker,
    modules => [population_mgr_worker]
  },
  {ok, _Pid} = supervisor:start_child(?MODULE, PopulationMgr).

%%-----------------------------------------------------------------------------
%% @doc Restarts populaton_monitor.
%% @end
%%-----------------------------------------------------------------------------
-spec restart_population_mgr() -> {ok, pid()}.
restart_population_mgr() ->
  {ok, _Pid} = supervisor:restart_child(?MODULE, population_mgr_worker).

%%%============================================================================
%%% Supervisor callbacks
%%%============================================================================

%%-----------------------------------------------------------------------------
%% @private
%% @doc Whenever a supervisor is started using supervisor:start_link,
%%      this function is called by the new process to find out about restart
%%      strategy, maximum restart frequency and child specifications. We also
%%      make the supervisor the owner of the DB to improve fault tolerance.
%% @end
%%-----------------------------------------------------------------------------
-spec init([]) -> {ok, {sup_flags(), child_spec() | []}}.
init([]) ->
  ets:new(population_status, [set, public, named_table,
    {write_concurrency, true}, {read_concurrency, true}]),
  ets:new(evaluations, [set, public, named_table,
    {write_concurrency, true}, {read_concurrency, true}]),
  ets:new(active_agents, [set, public, named_table,
    {write_concurrency, true}, {read_concurrency, true}]),
  ets:new(inactive_agents, [set, public, named_table,
    {write_concurrency, true}, {read_concurrency, true}]),
  SupFlags = #{
    strategy => rest_for_one,
    intensity => 4,
    period => 20
  },
  ChildSpecs = [],
  {ok, {SupFlags, ChildSpecs}}.

%%%============================================================================
%%% Internal functions
%%%============================================================================