%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%   Copyright (C) 2009 by Gene Sher, DXNN Research Group,
%%%   CorticalComputer@gmail.com
%%%
%%%   The original release of this source code was introduced and explained
%%%   in a book (available for purchase on Amazon) by Gene Sher:
%%%
%%%     Handbook of Neuroevolution Through Erlang. Springer 2012,
%%%     print ISBN: 978-1-4614-4462-6 ebook ISBN: 978-1-4614-4463-6.
%%%
%%%   Licensed under the Apache License, Version 2.0 (the "License");
%%%   you may not use this file except in compliance with the License.
%%%   You may obtain a copy of the License at
%%%
%%%     http://www.apache.org/licenses/LICENSE-2.0
%%%
%%%   Unless required by applicable law or agreed to in writing, software
%%%   distributed under the License is distributed on an "AS IS" BASIS,
%%%   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%%   See the License for the specific language governing permissions and
%%%   limitations under the License.
%%%%%%%%%%%%%%%%%%%% Deus Ex Neural Network :: DXNN %%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%   Modified Copyright (C) 2018 ApproximateReality
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%----------------------------------------------------------------------------
%%% @doc The Cortex is a NN synchronizing element. It needs to know the Pid of
%%%      every sensor and actuator, so that it will know when all the
%%%      actuators have received their control inputs, and that it's time for
%%%      the sensors to again gather and fanout sensory data to the neurons in
%%%      the input layer.
%%% @end
%%%----------------------------------------------------------------------------
-module(cortex).

%% Start/Stop
-export([
  start/2,
  stop/2
]).

%% API
-export([
  init_phase2/7,
  sync/4,
  reactivate/2
]).

%% Callbacks
-export([
  init/1,
  loop/1, loop/8,
  handle/2,
  terminate/1
]).

%% Xref
-ignore_xref([
  loop/1, loop/8,
  handle/2,
  terminate/1
]).

%%%============================================================================
%%% Types
%%%============================================================================

-record(cortex_state, {
  id :: models:cortex_id(),
  spids :: [pid()],
  npids :: [pid()],
  start_time :: integer(),
  goal_reached :: boolean()
}).

-type state() :: #cortex_state{}.

%%%============================================================================
%%% API
%%%============================================================================

%%-----------------------------------------------------------------------------
%% @doc Spawns a Cortex process belonging to the Exoself process that
%%      spawned it and calls init to initialize.
%% @end
%%-----------------------------------------------------------------------------
-spec start(node(), pid()) -> pid().
start(Node, ExoselfPid) ->
  spawn_link(Node, ?MODULE, init, [ExoselfPid]).

%%-----------------------------------------------------------------------------
%% @doc Terminates the cortex.
%% @end
%%-----------------------------------------------------------------------------
-spec stop(pid(), pid()) -> ok.
stop(Pid, ExoselfPid) ->
  Pid ! {ExoselfPid, stop},
  ok.

%%-----------------------------------------------------------------------------
%% @doc Initializes the cortex.
%% @end
%%-----------------------------------------------------------------------------
-spec init_phase2(pid(), pid(), models:cortex_id(), [pid()], [pid()], [pid()], models:op_mode())
-> ok.
init_phase2(Pid, ExoselfPid, Id, SPids, NPids, APids, OpMode) ->
  Pid ! {handle, {init_phase2, ExoselfPid, Id, SPids, NPids, APids, OpMode}},
  ok.

%%-----------------------------------------------------------------------------
%% @doc The Cortex's goal is to synchronize the the NN system such that when
%%      the actuators have received all their control signals, the sensors are
%%      once again triggered to gather new sensory information. Thus the
%%      cortex waits for the sync messages from the actuator Pids in its
%%      system, and once it has received all the sync messages, it triggers
%%      the sensors and then drops back to waiting for a new set of sync
%%      messages. The cortex stores 2 copies of the actuator Pids: the APids,
%%      and the MemoryAPids (MAPids). Once all the actuators have sent it the
%%      sync messages, it can restore the APids list from the MAPids. Finally,
%%      there is also the Step variable which decrements every time a full
%%      cycle of Sense-Think-Act completes, once this reaches 0, the NN system
%%      begins its termination and backup process.
%% @end
%%-----------------------------------------------------------------------------
-spec sync(pid(), pid(), [float()], integer() | goal_reached) -> ok.
sync(Pid, APid, Fitness, EndFlag) ->
  Pid ! {handle, {sync, APid, Fitness, EndFlag}},
  ok.

%%-----------------------------------------------------------------------------
%% @doc Reactivates the cortex resetting it to its initial state.
%% @end
%%-----------------------------------------------------------------------------
-spec reactivate(pid(), pid()) -> ok.
reactivate(Pid, ExoselfPid) ->
  Pid ! {handle, {ExoselfPid, reactivate}},
  ok.

%%%============================================================================
%%% Callbacks
%%%============================================================================

%%-----------------------------------------------------------------------------
%% @private
%% @doc Whenever a Cortex process is started via the start function this
%%      function is called by the new process to initialize.
%% @end
%%-----------------------------------------------------------------------------
-spec init(pid()) -> no_return.
init(ExoselfPid) ->
  utils:random_seed(),
  logr:debug({cortex, init, ok, undefined, []}),
  loop(ExoselfPid).

%%-----------------------------------------------------------------------------
%% @private
%% @doc Receive and handle messages.
%% @end
%%-----------------------------------------------------------------------------
-spec loop(pid()) -> no_return.
loop(ExoselfPid) ->
  receive
    {handle, {init_phase2, ExoselfPid, Id, SPids, NPids, APids, OpMode}} ->
      NewState = handle(init_phase2, {Id, SPids, NPids}),
      loop(NewState, ExoselfPid, {APids, APids}, 1, 0, 0, active, OpMode)
  end.

%%-----------------------------------------------------------------------------
%% @private
%% @doc Receive and handle messages.
%% @end
%%-----------------------------------------------------------------------------
-spec loop(state(), pid(), {[pid()] | [], [pid()]}, integer(), 0, integer(),
  active | inactive, models:op_mode()) -> no_return.
loop(State, ExoselfPid, {[APid | APids], MAPids}, CycleAcc, FitnessAcc, EFAcc, active, OpMode) ->
  receive
    {handle, {sync, APid, Fitness, EFlag}} ->
      UFitnessAcc = update_fitness_acc(FitnessAcc, Fitness, OpMode),
      case EFlag of
        goal_reached ->
          logr:info({cortex, status, ok, "syncd - goal_reached", []}),
          NewState = State#cortex_state{goal_reached = true},
          loop(NewState, ExoselfPid, {APids, MAPids}, CycleAcc,
            UFitnessAcc, EFAcc + 1, active, OpMode);
        _ ->
          logr:debug({cortex, msg, ok, "syncd", []}),
          loop(State, ExoselfPid, {APids, MAPids}, CycleAcc,
            UFitnessAcc, EFAcc + EFlag, active, OpMode)
      end;
    {ExoselfPid, stop} ->
      terminate(normal)
  end;

loop(State, ExoselfPid, {[], MAPids}, CycleAcc, FitnessAcc, EFAcc, active, OpMode) ->
  case EFAcc > 0 of
    true ->
      logr:debug({cortex, msg, ok, "all sync msgs received - evaluation finished", []}),
      StartTime = State#cortex_state.start_time,
      GoalReached = State#cortex_state.goal_reached,
      handle(evaluation_complete, {ExoselfPid, FitnessAcc, CycleAcc, StartTime, GoalReached}),
      loop(State, ExoselfPid, {MAPids, MAPids}, CycleAcc, FitnessAcc, EFAcc, inactive, OpMode);
    false ->
      logr:debug({cortex, msg, ok, "all sync msgs received - evaluation not finished", []}),
      handle(continue, State#cortex_state.spids),
      loop(State, ExoselfPid, {MAPids, MAPids}, CycleAcc + 1, FitnessAcc, EFAcc, active, OpMode)
  end;

loop(State, ExoselfPid, {MAPids, MAPids}, _CycleAcc, _FitnessAcc, _EFAcc, inactive, OpMode) ->
  receive
    {handle, {ExoselfPid, reactivate}} ->
      handle(reactivate, State#cortex_state.spids),
      logr:debug({cortex, msg, ok, "reactivated", []}),
      StartTime = erlang:monotonic_time(),
      NewState = State#cortex_state{start_time = StartTime},
      loop(NewState, ExoselfPid, {MAPids, MAPids}, 1, 0, 0, active, OpMode);
    {ExoselfPid, stop} ->
      terminate(normal)
  end.

%%-----------------------------------------------------------------------------
%% @private
%% @doc This function is called to terminate the process. It performs
%%      any necessary cleaning up before exiting with the << Reason >>
%%      parameter that it was called with.
%% @end
%%-----------------------------------------------------------------------------
-spec terminate(atom()) -> ok.
terminate(Reason) ->
  logr:debug({cortex, terminate, ok, undefined, []}),
  exit(Reason).

%%%============================================================================
%%% Internal functions
%%%============================================================================

handle(init_phase2, {Id, SPids, NPids}) ->
  StartTime = erlang:monotonic_time(),
  [sensor:sync(SPid, self()) || SPid <- SPids],
  logr:debug({cortex, init2, ok, undefined, []}),
  State = #cortex_state{
    id = Id,
    spids = SPids,
    npids = NPids,
    start_time = StartTime,
    goal_reached = false
  },
  State;
handle(evaluation_complete, {ExoselfPid, FitnessAcc, CycleAcc, StartTime, GoalReached}) ->
  TimeDif = (erlang:monotonic_time() - StartTime),
  exoself:evaluation_complete(ExoselfPid, self(), FitnessAcc, CycleAcc, TimeDif, GoalReached);
handle(continue, SPids) ->
  [sensor:sync(SPid, self()) || SPid <- SPids];
handle(reactivate, SPids) ->
  [sensor:sync(SPid, self()) || SPid <- SPids].

update_fitness_acc(FitnessAcc, Fitness, _OpMode) ->
  vector_add(Fitness, FitnessAcc, []).

vector_add(LA, 0, []) ->
  LA;
vector_add([A | LA], [B | LB], Acc) ->
  vector_add(LA, LB, [A + B | Acc]);
vector_add([], [], Acc) ->
  lists:reverse(Acc).
