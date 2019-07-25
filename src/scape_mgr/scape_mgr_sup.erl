%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Copyright (C) 2018 ApproximateReality
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%----------------------------------------------------------------------------
%%% @doc ScapeMgr top supervisor.
%%% @end
%%%----------------------------------------------------------------------------
-module(scape_mgr_sup).

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
  ets:new(scape_names_pids, [set, public, named_table,
    {write_concurrency, true}, {read_concurrency, true}]),
  SupFlags = #{
    strategy => one_for_all,
    intensity => 4,
    period => 20
  },
  SectorSup = #{
    id => sector_sup,
    start => {sector_sup, start_link, []},
    restart => permanent,
    shutdown => infinity,
    type => supervisor,
    modules => [sector_sup]
  },
  ScapeSup = #{
    id => scape_sup,
    start => {scape_sup, start_link, []},
    restart => permanent,
    shutdown => infinity,
    type => supervisor,
    modules => [scape_sup]
  },
  ScapeMgr = #{
    id => scape_mgr,
    start => {scape_mgr, start_link, []},
    restart => permanent,
    shutdown => 5000,
    type => worker,
    modules => [scape_mgr]
  },
  {ok, {SupFlags, [SectorSup, ScapeSup, ScapeMgr]}}.

%%%============================================================================
%%% Internal functions
%%%============================================================================