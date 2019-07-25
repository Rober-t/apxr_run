%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Copyright (C) 2018 ApproximateReality
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%----------------------------------------------------------------------------
%%% @doc Sector supervisor.
%%% @end
%%%----------------------------------------------------------------------------
-module(sector_sup).

-behaviour(supervisor).

%% Start/Stop
-export([
  start_link/0
]).

%% API
-export([
  start_sector/2
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
%% @doc The start_sector starts a new sector process.
%% @end
%%-----------------------------------------------------------------------------
-spec start_sector(atom(), atom() | integer()) -> {ok, pid()}.
start_sector(ModName, UId) ->
  {ok, _Pid} = supervisor:start_child(?MODULE, [ModName, UId]).

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
  SupFlags = #{
    strategy => simple_one_for_one,
    intensity => 6,
    period => 30
  },
  Sector = #{
    id => sector,
    start => {sector, start_link, []},
    restart => transient,
    shutdown => 5000,
    type => worker,
    modules => [sector]
  },
  ChildSpecs = [Sector],
  {ok, {SupFlags, ChildSpecs}}.

%%%============================================================================
%%% Internal functions
%%%============================================================================