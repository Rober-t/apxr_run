%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Copyright (C) 2018 ApproximateReality
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%----------------------------------------------------------------------------
%%% @doc ApxrRun top level application.
%%% @end
%%%----------------------------------------------------------------------------
-module(apxr_run_app).

-behaviour(application).

%% Application callbacks
-export([
  start/2,
  stop/1
]).

%%%============================================================================
%%% API
%%%============================================================================

%%-----------------------------------------------------------------------------
%% @private
%% @doc This function is called whenever an application is started using
%%      application:start, and should start the processes of the
%%      application. Since this application is structured according to the OTP
%%      design principles as a supervision tree, this means starting the
%%      top supervisor of the tree.
%% @end
%%-----------------------------------------------------------------------------
-spec start(normal | {takeover, node()} | {failover, node()}, any()) -> {ok, pid()}.
start(_StartType, _StartArgs) ->
  apxr_run_sup:start_link().

%%-----------------------------------------------------------------------------
%% @private
%% @doc This function is called whenever an application has stopped. It
%%      is intended to be the opposite of Module:start and should do
%%      any necessary cleaning up. The return value is ignored.
%% @end
%%-----------------------------------------------------------------------------
-spec stop(term()) -> ok.
stop(_State) ->
  init:stop().

%%%============================================================================
%%% Internal functions
%%%============================================================================
