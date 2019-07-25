%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Copyright (C) 2018 ApproximateReality
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%----------------------------------------------------------------------------
%%% @doc Scape supervisor.
%%% @end
%%%----------------------------------------------------------------------------
-module(scape_sup).

-behaviour(supervisor).

%% Start/Stop
-export([
  start_link/0
]).

%% API
-export([
  start_scape/5,
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
%% @doc The start_scape starts a new scape process.
%% @end
%%-----------------------------------------------------------------------------
-spec start_scape(X :: float(), Y :: float(), W :: float(), H :: float(), ModName :: atom())
-> {ok, pid()}.
start_scape(X, Y, Width, Height, ModName) ->
  {ok, _Pid} = supervisor:start_child(?MODULE, [X, Y, Width, Height, ModName]).

%%-----------------------------------------------------------------------------
%% @doc Stops the scape process in accordance with the shutdown
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
-spec init([]) -> {ok, {scape_mgr_sup:sup_flags(), scape_mgr_sup:child_spec()}}.
init([]) ->
  ets:new(ids_sids_loc, [set, public, named_table,
    {write_concurrency, true}, {read_concurrency, true}]),
  ets:new(xy_pts, [set, public, named_table,
    {write_concurrency, true}, {read_concurrency, true}]),
  ets:new(qt, [set, public, named_table,
    {write_concurrency, true}, {read_concurrency, true}]),
  SupFlags = #{
    strategy => simple_one_for_one,
    intensity => 6,
    period => 30
  },
  Scape = #{
    id => scape,
    start => {scape, start_link, []},
    restart => transient,
    shutdown => 5000,
    type => worker,
    modules => [scape]
  },
  ChildSpecs = [Scape],
  {ok, {SupFlags, ChildSpecs}}.

%%%============================================================================
%%% Internal functions
%%%============================================================================