%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Copyright (C) 2018 ApproximateReality
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%----------------------------------------------------------------------------
%%% @doc Agent supervisor.
%%% @end
%%%----------------------------------------------------------------------------
-module(agent_sup).

-behaviour(supervisor).

%% Start/Stop
-export([
  start_link/0,
  stop_agent/1
]).

%% API
-export([
  start_agent/2
]).

%% supervisor callbacks
-export([
  init/1
]).

%% Xref
-ignore_xref([
  start_link/0
]).

%%%============================================================================
%%% API
%%%============================================================================

%%-----------------------------------------------------------------------------
%% @doc Starts the supervisor server.
%% @end
%%-----------------------------------------------------------------------------
-spec start_link() -> {ok, pid()}.
start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%-----------------------------------------------------------------------------
%% @doc start_agent starts a new agent_worker process.
%% @end
%%-----------------------------------------------------------------------------
-spec start_agent(models:agent_id(), models:op_mode()) -> {ok, pid()}.
start_agent(AgentId, OpMode) ->
  {ok, _Pid} = supervisor:start_child(?MODULE, [AgentId, OpMode]).

%%-----------------------------------------------------------------------------
%% @doc Stops an agent_worker process in accordance with the shutdown
%%      specification.
%% @end
%%-----------------------------------------------------------------------------
-spec stop_agent(pid()) -> ok.
stop_agent(Pid) ->
  supervisor:terminate_child(?MODULE, Pid).

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
-spec init([]) -> {ok, {agent_mgr_sup:sup_flags(), agent_mgr_sup:child_spec()}}.
init([]) ->
  SupFlags = #{
    strategy => simple_one_for_one,
    intensity => 6,
    period => 30
  },
  AgentWorker = #{
    id => agent_worker,
    start => {agent_worker, start_link, []},
    restart => transient,
    shutdown => 5000,
    type => worker,
    modules => [agent_worker]
  },
  ChildSpecs = [AgentWorker],
  {ok, {SupFlags, ChildSpecs}}.

%%%============================================================================
%%% Internal functions
%%%============================================================================