%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Copyright (C) 2018 ApproximateReality
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%----------------------------------------------------------------------------
%%% @doc Logr - logger wrapper and associated functions e.g. filters.
%%% @end
%%%----------------------------------------------------------------------------
-module(logr).

%-include_lib("kernel/include/logger.hrl").

%% API
-export([
  debug/1,
  info/1,
  notice/1,
  warning/1,
  error/1,
  critical/1,
  alert/1,
  emergency/1
]).

%% Logger filters
-export([
  scape_filter/2,
  status_filter/2
]).

%% Xref
-ignore_xref([
  debug/1,
  info/1,
  notice/1,
  warning/1,
  error/1,
  critical/1,
  alert/1,
  emergency/1,
  scape_filter/2,
  status_filter/2
]).

%%%============================================================================
%%% Configuration
%%%============================================================================

-define(IS_ACTION(A), (A==log orelse A==stop)).

%%=============================================================================
%% Types
%%=============================================================================

-type log_body() :: {
  In :: atom(), % The subcomponent taking action and logging data
  What :: atom(), % A value defining the purpose
  Result :: ok | error, % The result of a given operation being reported
  Details :: string() | undefined, % Additional information explaining the result
  Params :: [any()] | [] % Additional parameters for the above.
}.

%%%============================================================================
%%% API
%%%============================================================================

%%-----------------------------------------------------------------------------
%% @doc Logs message with level debug.
%% @end
%%-----------------------------------------------------------------------------
-spec debug(log_body()) -> ok.
debug({In, What, Result, Details, Params}) ->
  % io:format("~p~n", [#{
  %   level => debug,
  %   in => In,
  %   what => What,
  %   result => Result,
  %   details => Details,
  %   params => list_to_tuple(Params)
  % }]).
  ok.


%%-----------------------------------------------------------------------------
%% @doc Logs message with level info.
%% @end
%%-----------------------------------------------------------------------------
-spec info(log_body()) -> ok.
info({In, What, Result, Details, Params}) ->
  io:format("~p~n", [#{
    level => info,
    in => In,
    what => What,
    result => Result,
    details => Details,
    params => list_to_tuple(Params)
  }]).

%%-----------------------------------------------------------------------------
%% @doc Logs message with level notice.
%% @end
%%-----------------------------------------------------------------------------
-spec notice(log_body()) -> ok.
notice({In, What, Result, Details, Params}) ->
  io:format("~p~n", [#{
    level => notice,
    in => In,
    what => What,
    result => Result,
    details => Details,
    params => list_to_tuple(Params)
  }]).

%%-----------------------------------------------------------------------------
%% @doc Logs message with level warning.
%% @end
%%-----------------------------------------------------------------------------
-spec warning(log_body()) -> ok.
warning({In, What, Result, Details, Params}) ->
  io:format("~p~n", [#{
    level => warning,
    in => In,
    what => What,
    result => Result,
    details => Details,
    params => list_to_tuple(Params)
  }]).

%%-----------------------------------------------------------------------------
%% @doc Logs message with level error.
%% @end
%%-----------------------------------------------------------------------------
-spec error(log_body()) -> ok.
error({In, What, Result, Details, Params}) ->
  io:format("~p~n", [#{
    level => error,
    in => In,
    what => What,
    result => Result,
    details => Details,
    params => list_to_tuple(Params)
  }]).

%%-----------------------------------------------------------------------------
%% @doc Logs message with level critical.
%% @end
%%-----------------------------------------------------------------------------
-spec critical(log_body()) -> ok.
critical({In, What, Result, Details, Params}) ->
  io:format("~p~n", [#{
    level => critical,
    in => In,
    what => What,
    result => Result,
    details => Details,
    params => list_to_tuple(Params)
  }]).

%%-----------------------------------------------------------------------------
%% @doc Logs message with level alert.
%% @end
%%-----------------------------------------------------------------------------
-spec alert(log_body()) -> ok.
alert({In, What, Result, Details, Params}) ->
  io:format("~p~n", [#{
    level => alert,
    in => In,
    what => What,
    result => Result,
    details => Details,
    params => list_to_tuple(Params)
  }]).

%%-----------------------------------------------------------------------------
%% @doc Logs message with level emergency.
%% @end
%%-----------------------------------------------------------------------------
-spec emergency(log_body()) -> ok.
emergency({In, What, Result, Details, Params}) ->
  io:format("~p~n", [#{
    level => emergency,
    in => In,
    what => What,
    result => Result,
    details => Details,
    params => list_to_tuple(Params)
  }]).

%%%============================================================================
%%% Logger filters
%%%============================================================================

%%-----------------------------------------------------------------------------
%% @doc Filter audit logs.
%% @end
%%-----------------------------------------------------------------------------
-spec scape_filter(logger:log_event(), log | stop) -> logger:filter_return().
scape_filter(LogEvent, Action) when ?IS_ACTION(Action) ->
  filter_scape(LogEvent, on_match(Action, LogEvent));
scape_filter(LogEvent, Action) ->
  erlang:error(badarg, [LogEvent, Action]).

%%-----------------------------------------------------------------------------
%% @doc Filter status logs.
%% @end
%%-----------------------------------------------------------------------------
-spec status_filter(logger:log_event(), log | stop) -> logger:filter_return().
status_filter(LogEvent, Action) when ?IS_ACTION(Action) ->
  filter_status(LogEvent, on_match(Action, LogEvent));
status_filter(LogEvent, Action) ->
  erlang:error(badarg, [LogEvent, Action]).

%%%============================================================================
%%% Internal functions
%%%============================================================================

filter_scape(#{msg := {report, #{in := scape}}}, OnMatch) ->
  OnMatch;
filter_scape(_, _) ->
  ignore.

filter_status(#{msg := {report, #{what := status}}}, OnMatch) ->
  OnMatch;
filter_status(#{msg := {report, #{result := error}}}, OnMatch) ->
  OnMatch;
filter_status(_, _) ->
  ignore.

on_match(log, LogEvent) ->
  LogEvent;
on_match(stop, _) ->
  stop.