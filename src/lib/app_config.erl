%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Copyright (C) 2018 ApproximateReality
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%----------------------------------------------------------------------------
%%% @doc Application configuration utilities.
%%% @end
%%%----------------------------------------------------------------------------
-module(app_config).

%% API
-export([
  get_env/1, get_env/2,
  get_all/0, get_all/1
]).

%% Xref
-ignore_xref([
  get_env/1, get_env/2,
  get_all/0, get_all/1
]).

%%%============================================================================
%%% API
%%%============================================================================

%%-----------------------------------------------------------------------------
%% @doc Retrieves value from application environment from the Polis keyspace.
%% @end
%%-----------------------------------------------------------------------------
-spec get_env(atom()) -> any().
get_env(Key) ->
  app_config:get_env(apxr_run, Key).

%%-----------------------------------------------------------------------------
%% @doc Retrieves value from application environment from the given keyspace.
%% @end
%%-----------------------------------------------------------------------------
-spec get_env(atom(), atom()) -> any().
get_env(Keyspace, Key) ->
  element(2, {ok, _} = application:get_env(Keyspace, Key)).

%%-----------------------------------------------------------------------------
%% @doc Retrieves all application config values from the Polis keyspace.
%% @end
%%-----------------------------------------------------------------------------
-spec get_all() -> [{atom(), _}].
get_all() ->
  application:get_all_env(apxr_run).

%%-----------------------------------------------------------------------------
%% @doc Retrieves all application config values from the given keyspace.
%% @end
%%-----------------------------------------------------------------------------
-spec get_all(atom()) -> [{atom(), _}].
get_all(Keyspace) ->
  application:get_all_env(Keyspace).

%%%============================================================================
%%% Internal functions
%%%============================================================================