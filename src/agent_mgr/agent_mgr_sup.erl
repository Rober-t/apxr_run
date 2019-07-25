%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Copyright (C) 2018 ApproximateReality
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%----------------------------------------------------------------------------
%%% @doc AgentMgr top level supervisor.
%%% @end
%%%----------------------------------------------------------------------------
-module(agent_mgr_sup).

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
%%      strategy, maximum restart frequency and child specifications. We also
%%      make the supervisor the owner of an ETS table to improve fault
%%      tolerance.
%% @end
%%-----------------------------------------------------------------------------
-spec init([]) -> {ok, {sup_flags(), child_spec()}}.
init([]) ->
  ets:new(agent_ids_pids, [set, public, named_table,
    {write_concurrency, true}, {read_concurrency, true}]),
  SupFlags = #{
    strategy => rest_for_one,
    intensity => 4,
    period => 20
  },
  PrivateScapeSup = #{
    id => private_scape_sup,
    start => {private_scape_sup, start_link, []},
    restart => permanent,
    shutdown => infinity,
    type => supervisor,
    modules => [private_scape_sup]
  },
  AgentSup = #{
    id => agent_sup,
    start => {agent_sup, start_link, []},
    restart => permanent,
    shutdown => infinity,
    type => supervisor,
    modules => [agent_sup]
  },
  AgentMgr = #{
    id => agent_mgr,
    start => {agent_mgr, start_link, []},
    restart => permanent,
    shutdown => 5000,
    type => worker,
    modules => [agent_mgr]
  },
  ChildSpecs = [PrivateScapeSup, AgentSup, AgentMgr],
  {ok, {SupFlags, ChildSpecs}}.

%%%============================================================================
%%% Internal functions
%%%============================================================================