%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Copyright (C) 2018 ApproximateReality
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%----------------------------------------------------------------------------
%%% @doc PrivateScape supervisor.
%%% @end
%%%----------------------------------------------------------------------------
-module(private_scape_sup).

-behaviour(supervisor).

%% Start/Stop
-export([
  start_link/0
]).

%% API
-export([
  start_scape/2,
  stop_scape/1
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
%% @doc Starts the supervisor.
%% @end
%%-----------------------------------------------------------------------------
-spec start_link() -> {ok, pid()}.
start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%-----------------------------------------------------------------------------
%% @doc The start_scape starts a new private_scape process.
%% @end
%%-----------------------------------------------------------------------------
-spec start_scape(models:agent_id(), atom()) -> {ok, pid()}.
start_scape(AgentId, ModName) ->
  {ok, _Pid} = supervisor:start_child(?MODULE, [AgentId, ModName]).

%%-----------------------------------------------------------------------------
%% @doc Stops the private_scape process in accordance with the shutdown
%%      specification.
%% @end
%%-----------------------------------------------------------------------------
-spec stop_scape(pid()) -> ok.
stop_scape(Pid) ->
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
  % % (FX Application)
  % ets:new(metadata, [set, public, named_table, {keypos, 2}]),
  % ets:new('EURUSD15', [ordered_set, public, named_table, {keypos, 2}]),
  SupFlags = #{
    strategy => simple_one_for_one,
    intensity => 6,
    period => 30
  },
  PrivateScape = #{
    id => private_scape,
    start => {private_scape, start_link, []},
    restart => transient,
    shutdown => 5000,
    type => worker,
    modules => [private_scape]
  },
  ChildSpecs = [PrivateScape],
  {ok, {SupFlags, ChildSpecs}}.

%%%============================================================================
%%% Internal functions
%%%============================================================================