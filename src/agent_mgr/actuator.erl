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
%%% @doc Defines generic actuator behavior.
%%%      An actuator is a process that accepts signals from neurons (see
%%%      selective receive forward message) in the output layer, orders them
%%%      into a vector, and then uses this vector to control some function
%%%      that acts on the environment or even the NN itself. An actuator might
%%%      have incoming connections from 3 neurons, in which case it would
%%%      then have to wait until all 3 of the neurons have sent it their output
%%%      signals, accumulate these signals into a vector, and then use this
%%%      vector as a parameter to its actuation function. The order in which
%%%      the signals are accumulated into a vector is in the same order as the
%%%      neuron ids are stored with NIds. Once all the signals have been
%%%      gathered, the actuator executes its function, waits for its fitness
%%%      score from the scape, sends the cortex the sync signal, and then
%%%      again begins to wait for the neural signals from the output layer by
%%%      reseting the FaninPids.
%%% @end
%%%----------------------------------------------------------------------------
-module(actuator).

%% Start/Stop
-export([
  start/2,
  stop/2
]).

%% API
-export([
  init_phase2/11,
  fitness/2
]).

%% Callbacks
-export([
  init/1,
  loop/1, loop/6,
  handle/2,
  terminate/2
]).

%% Xref
-ignore_xref([
  loop/1, loop/6,
  handle/2,
  terminate/2,
  behaviour_info/1
]).

%%%============================================================================
%%% Types
%%%============================================================================

-record(actuator_state, {
  id :: models:actuator_id(),
  agent_id :: models:agent_id(),
  scape :: pid() | atom(),
  name :: {atom(), atom()},
  mod_state :: mod_state(),
  vl :: non_neg_integer(),
  params :: any()
}).

-type state() :: #actuator_state{}.
-type mod_state() :: any().

%%%============================================================================
%%% Behavior
%%%============================================================================

-callback init([]) -> {ok, InitialModState :: mod_state()}.

-callback actuate(Name :: atom(), {AgentId :: models:agent_id(), Output :: [float()],
  Params :: any(), VL :: non_neg_integer(), Scape :: pid() | atom(),
  Actuator :: pid() | models:actuator_id(), ModState :: mod_state()}) -> ModState :: mod_state().

-callback terminate(Reason :: atom(), ModState :: mod_state()) ->
  ok.

-optional_callbacks([
  terminate/2
]).

%%%============================================================================
%%% API
%%%============================================================================

%%-----------------------------------------------------------------------------
%% @doc Spawns an Actuator process belonging to the exoself process that
%%      spawned it and calls init to initialize.
%% @end
%%-----------------------------------------------------------------------------
-spec start(node(), pid()) -> pid().
start(Node, ExoselfPid) ->
  spawn_link(Node, ?MODULE, init, [ExoselfPid]).

%%-----------------------------------------------------------------------------
%% @doc Terminates the actuator.
%% @end
%%-----------------------------------------------------------------------------
-spec stop(pid(), pid()) -> ok.
stop(Pid, ExoselfPid) ->
  Pid ! {ExoselfPid, stop}, ok.

%%-----------------------------------------------------------------------------
%% @doc Initializes the actuator setting it to its initial state.
%% @end
%%-----------------------------------------------------------------------------
-spec init_phase2(pid(), pid(), models:actuator_id(), models:agent_id(), pid(), pid(),
  {atom(), atom()}, integer(), any(), [pid()], models:op_mode()) -> ok.
init_phase2(Pid, ExoselfPid, Id, AgentId, CxPid, Scape, AName, VL, Params, FaninPids, OpMode) ->
  Pid ! {handle, {init_phase2, ExoselfPid, Id, AgentId, CxPid, Scape, AName, VL, Params,
  FaninPids, OpMode}}, ok.

%%----------------------------------------------------------------------------
%% @doc The fitness score from the scape after the actuator has performed an
%%      action.
%% @end
%%----------------------------------------------------------------------------
-spec fitness(pid(), {[float()], integer() | atom()}) -> ok.
fitness(ActuatorPid, {Fitness, HaltFlag}) ->
  ActuatorPid ! {handle, {fitness, {Fitness, HaltFlag}}}, ok.

%%%============================================================================
%%% Callbacks
%%%============================================================================

%%-----------------------------------------------------------------------------
%% @private
%% @doc Whenever an Actuator process is started via the start function this
%%      function is called by the new process to initialize.
%% @end
%%-----------------------------------------------------------------------------
-spec init(pid()) -> no_return().
init(ExoselfPid) ->
  process_flag(trap_exit, true),
  logr:debug({actuator, init, ok, undefined, []}),
  loop(ExoselfPid).

%%-----------------------------------------------------------------------------
%% @private
%% @doc Receive and handle messages.
%% @end
%%-----------------------------------------------------------------------------
-spec loop(pid()) -> no_return().
loop(ExoselfPid) ->
  receive
    {handle, {init_phase2, ExoselfPid, Id, AgentId, CxPid, Scape, AName, VL, Params,
    FaninPids, OpMode}} ->
      NewState = handle(init_phase2, {Id, AgentId, Scape, AName, VL, Params}),
      loop(NewState, ExoselfPid, CxPid, {FaninPids, FaninPids}, [], OpMode)
  end.

%%-----------------------------------------------------------------------------
%% @private
%% @doc Receive and handle messages.
%% @end
%%-----------------------------------------------------------------------------
-spec loop(state(), pid(), pid(), {[pid()] | [], [pid()]}, [float()], models:op_mode())
-> no_return().
loop(S, ExoselfPid, CxPid, {[FromPid | FaninPids], MFaninPids}, Acc, OpMode) ->
  receive
    {forward, FromPid, Input} ->
      logr:debug({actuator, msg, ok, "forward received", [FromPid]}),
      loop(S, ExoselfPid, CxPid, {FaninPids, MFaninPids}, lists:append(Input, Acc), OpMode);
    {'EXIT', _Pid, normal} ->
      ignore;
    {'EXIT', Pid, Reason} ->
      logr:debug({actuator, msg, ok, "exit received", [Pid, Reason]}),
      terminate(Reason, S);
    {ExoselfPid, stop} ->
      terminate(normal, S)
  end;

loop(S, ExoselfPid, CxPid, {[], MFaninPids}, Acc, OpMode) ->
  AName = S#actuator_state.name,
  ActuatorId = S#actuator_state.id,
  ModState = S#actuator_state.mod_state,
  Params = S#actuator_state.params,
  VL = S#actuator_state.vl,
  Scape = S#actuator_state.scape,
  AgentId = S#actuator_state.agent_id,
  NewModS = handle(actuate, {AgentId, CxPid, Acc, AName, Params, VL, Scape, ActuatorId, ModState}),
  receive
    {handle, {fitness, {Fitness, HaltFlag}}} ->
      handle(fitness, {Fitness, HaltFlag, CxPid, OpMode}),
      loop(S#actuator_state{mod_state = NewModS}, ExoselfPid,
        CxPid, {MFaninPids, MFaninPids}, [], OpMode)
    after 30000 ->
      logr:warning({actuator, msg, error, "fitness not received", []})
  end.

%%-----------------------------------------------------------------------------
%% @private
%% @doc This function is called to terminate the process. It performs
%%      any necessary cleaning up before exiting with the << Reason >>
%%      parameter that it was called with.
%% @end
%%-----------------------------------------------------------------------------
-spec terminate(atom(), state()) -> ok.
terminate(Reason, State) ->
  {Mod, _Name} = State#actuator_state.name,
  case erlang:function_exported(Mod, terminate, 2) of
    true ->
      M = utils:get_module(Mod),
      M:terminate(Reason, State#actuator_state.mod_state);
    false ->
      ok
  end,
  logr:debug({actuator, terminate, ok, undefined, [Reason]}),
  exit(Reason).

%%%============================================================================
%%% Internal functions
%%%============================================================================

handle(init_phase2, {Id, AgentId, Scape, {Mod, Name}, VL, Params}) ->
  M = utils:get_module(Mod),
  {ok, ModState} = M:init([]),
  logr:debug({actuator, init2, ok, undefined, []}),
  State = #actuator_state{
    id = Id,
    agent_id = AgentId,
    scape = Scape,
    name = {Mod, Name},
    mod_state = ModState,
    vl = VL,
    params = Params
  },
  State;

handle(actuate, {AgentId, CxPid, Acc, {Mod, Name}, Params, VL, Scape, AId, ModState}) ->
  logr:debug({actuator, actuate, ok, "actuating", [CxPid]}),
  M = utils:get_module(Mod),
  M:actuate(Name, {AgentId, lists:reverse(Acc), Params, VL, Scape, AId, ModState});

handle(fitness, {Fitness, HaltFlag, CxPid, _OpMode}) ->
  logr:debug({actuator, fitness, ok, "syncing", [CxPid]}),
  cortex:sync(CxPid, self(), Fitness, HaltFlag).