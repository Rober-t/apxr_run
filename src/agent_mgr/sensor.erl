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
%%% @doc Defines generic sensor behavior.
%%%      A sensor is any process that produces a vector signal that the NN then
%%%      processes. This signal can be produced from the sensor interacting
%%%      with the environment, for example the data coming from a camera, or
%%%      from the sensor somehow generating the signal internally.
%%%      In real implementations NNs have to interact with the real or
%%%      simulated world, and the signals they produce need to be somehow
%%%      used to accomplish useful tasks and act upon those real or simulated
%%%      worlds. For example, our own brain accepts signals from the outside
%%%      world, and signals from our own body through the various sensory
%%%      organs, and the embedded sensory neurons within those organs. These
%%%      sensory organs, these sensors, encode the signals in a form that
%%%      can be forwarded to, and understood by, the brain. The output signals
%%%      produced by our brains also  have no action without some actuators to
%%%      interpret those signals, and then use those signals to act upon the
%%%      world. Thus, though it is the NN that thinks, it is the NN with
%%%      sensors and actuators that forms the whole system. It is the same
%%%      with artificial NNs. They require sensors, and actuators. A sensor
%%%      can be a camera, which can package its output signals in a way that
%%%      can be understood by the NN, for example by representing the sensory
%%%      signals as vectors. An actuator can be a motor, with a function that
%%%      can translate the NN’s output vector into electrical signals that
%%%      controls the actual motor. Thus it is the whole thing, the sensors
%%%      connected to and sending the sensory signals to the NN, and the NN
%%%      connected to and sending its output signals to the actuators, that
%%%      forms the full system. We refer to this system as a NN based system
%%%      or NN based agent.
%%% @end
%%%----------------------------------------------------------------------------
-module(sensor).

%% Start/Stop
-export([
  start/2,
  stop/2
]).

%% API
-export([
  init_phase2/11,
  sync/2,
  percept/2
]).

%% Callbacks
-export([
  init/1,
  loop/1, loop/4, loop/5,
  handle/2,
  terminate/2
]).

%% Xref
-ignore_xref([
  loop/1, loop/4, loop/5,
  handle/2,
  terminate/2,
  behaviour_info/1
]).

%%%============================================================================
%%% Types
%%%============================================================================

-record(sensor_state, {
  id :: models:sensor_id(),
  agent_id :: models:agent_id(),
  scape :: pid() | atom(),
  name :: {atom(), atom()},
  mod_state :: mod_state(),
  vl :: integer(),
  params :: any(),
  fanout_pids :: [pid()]
}).

-type state() :: #sensor_state{}.
-type mod_state() :: any().

%%%============================================================================
%%% Behavior
%%%============================================================================

-callback init([]) -> {ok, InitialModState :: mod_state()}.

-callback sense(Name :: atom(), {AgentId :: models:agent_id(), VL :: integer(),
  Params :: any(), Scape :: pid() | atom(), SensorId :: models:sensor_id(),
  OpMode :: models:op_mode(), ModState :: mod_state()}) -> ok.

-callback percept(Name :: atom(), {SensoryInput :: [float()] | atom(), AgentId :: models:agent_id(),
  VL :: integer(), Params :: any(), ModState :: mod_state()}) ->
  {SensorVector :: [float()], NewModState :: mod_state()}.

-callback terminate(Reason :: atom(), ModState :: mod_state()) ->
  ok.

-optional_callbacks([
  terminate/2
]).

%%%============================================================================
%%% API
%%%============================================================================

%%-----------------------------------------------------------------------------
%% @doc Spawns a Sensor process belonging to the exoself process that
%%      spawned it and calls init to initialize.
%% @end
%%-----------------------------------------------------------------------------
-spec start(node(), pid()) -> pid().
start(Node, ExoselfPid) ->
  spawn_link(Node, ?MODULE, init, [ExoselfPid]).

%%-----------------------------------------------------------------------------
%% @doc Terminates the Sensor.
%% @end
%%-----------------------------------------------------------------------------
-spec stop(pid(), pid()) -> ok.
stop(Pid, ExoselfPid) ->
  Pid ! {ExoselfPid, stop},
  ok.

%%-----------------------------------------------------------------------------
%% @doc Initializes the sensor setting it to its initial state.
%% @end
%%-----------------------------------------------------------------------------
-spec init_phase2(pid(), pid(), models:sensor_id(), models:agent_id(), pid(), pid(), atom(),
  integer(), any(), [pid()], models:op_mode()) -> ok.
init_phase2(Pid, ExoselfPid, Id, AgentId, CxPid, Scape, SName, VL, Params, FanoutPids, OpMode) ->
  Pid ! {handle, {init_phase2, ExoselfPid, Id, AgentId, CxPid, Scape, SName, VL, Params,
  FanoutPids, OpMode}},
  ok.

%%----------------------------------------------------------------------------
%% @doc sync triggers the sensor to begin gathering sensory data based on
%%      its sensory role.
%% @end
%%----------------------------------------------------------------------------
-spec sync(pid(), pid()) -> ok.
sync(Pid, CxPid) ->
  Pid ! {handle, {sync, CxPid}},
  ok.

%%----------------------------------------------------------------------------
%% @doc A percept is the input that an intelligent agent is perceiving at any
%%      given moment. This is sent from the scape in response to a sense
%%      request which is triggered whenever the sync function is called.
%% @end
%%----------------------------------------------------------------------------
-spec percept(pid(), [float()]) -> ok.
percept(Pid, Percept) ->
  Pid ! {handle, {percept, Percept}},
  ok.

%%%============================================================================
%%% Callbacks
%%%============================================================================

%%-----------------------------------------------------------------------------
%% @private
%% @doc Whenever a Sensor process is started via the start function this
%%      function is called by the new process to initialize.
%% @end
%%-----------------------------------------------------------------------------
-spec init(pid()) -> no_return().
init(ExoselfPid) ->
  process_flag(trap_exit, true),
  logr:debug({sensor, init, ok, undefined, []}),
  loop(ExoselfPid).

%%-----------------------------------------------------------------------------
%% @private
%% @doc Receive and handle messages.
%% @end
%%-----------------------------------------------------------------------------
-spec loop(pid()) -> no_return().
loop(ExoselfPid) ->
  receive
    {handle, {init_phase2, ExoselfPid, Id, AgentId, CxPid, Scape, SName, VL, Params,
    FanoutPids, OpMode}} ->
      NewState = handle(init_phase2, {Id, AgentId, Scape, SName, VL, Params, FanoutPids}),
      loop(NewState, ExoselfPid, CxPid, OpMode)
  end.

%%-----------------------------------------------------------------------------
%% @private
%% @doc Receive and handle messages.
%% @end
%%-----------------------------------------------------------------------------
-spec loop(state(), pid(), pid(), models:op_mode()) -> no_return().
loop(S, ExoselfPid, CxPid, OpMode) ->
  receive
    {handle, {sync, CxPid}} ->
      Scape = S#sensor_state.scape,
      SensorId = S#sensor_state.id,
      SName = S#sensor_state.name,
      ModState = S#sensor_state.mod_state,
      VL = S#sensor_state.vl,
      Params = S#sensor_state.params,
      AgentId = S#sensor_state.agent_id,
      handle(sync, {AgentId, Scape, SensorId, SName, VL, Params, OpMode, ModState}),
      loop(S, ExoselfPid, CxPid, OpMode, await_percept);
    {'EXIT', _Pid, normal} ->
      ignore;
    {'EXIT', Pid, Reason} ->
      logr:debug({sensor, msg, ok, "exit received", [Pid, Reason]}),
      terminate(Reason, S);
    {ExoselfPid, stop} ->
      terminate(normal, S)
  end.

%%-----------------------------------------------------------------------------
%% @private
%% @doc Receive and handle messages.
%% @end
%%-----------------------------------------------------------------------------
-spec loop(state(), pid(), pid(), models:op_mode(), await_percept) -> no_return().
loop(S, ExoselfPid, CxPid, OpMode, await_percept) ->
  receive
    {handle, {percept, SInput}} ->
      SName = S#sensor_state.name,
      ModState = S#sensor_state.mod_state,
      FanoutPids = S#sensor_state.fanout_pids,
      VL = S#sensor_state.vl,
      Params = S#sensor_state.params,
      AgentId = S#sensor_state.agent_id,
      UMState = handle(percept, {SInput, AgentId, SName, FanoutPids, VL, Params, ModState}),
      loop(S#sensor_state{mod_state = UMState}, ExoselfPid, CxPid, OpMode)
    after 30000 ->
      logr:warning({sensor, msg, error, "percept not received", []})
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
  {Mod, _Name} = State#sensor_state.name,
  case erlang:function_exported(Mod, terminate, 2) of
    true ->
      M = utils:get_module(Mod),
      M:terminate(Reason, State#sensor_state.mod_state);
    false ->
      ok
  end,
  logr:debug({sensor, terminate, ok, undefined, [Reason]}),
  exit(Reason).

%%%============================================================================
%%% Internal functions
%%%============================================================================

handle(init_phase2, {Id, AgentId, Scape, {Mod, Name}, VL, Params, FanoutPids}) ->
  M = utils:get_module(Mod),
  {ok, ModState} = M:init([]),
  logr:debug({sensor, init2, ok, undefined, [FanoutPids]}),
  State = #sensor_state{
    id = Id,
    agent_id = AgentId,
    scape = Scape,
    name = {Mod, Name},
    mod_state = ModState,
    vl = VL,
    params = Params,
    fanout_pids = FanoutPids
  },
  State;
handle(sync, {AgentId, Scape, SensorId, {Mod, Name}, VL, Params, OpMode, ModState}) ->
  logr:debug({sensor, sync, ok, "sensing", []}),
  M = utils:get_module(Mod),
  M:sense(Name, {AgentId, VL, Params, Scape, SensorId, OpMode, ModState});
handle(percept, {SensoryInput, AgentId, {Mod, Name}, FanoutPids, VL, Params, ModState}) ->
  M = utils:get_module(Mod),
  {SensorVector, NewModState} = M:percept(Name, {SensoryInput, AgentId, VL, Params, ModState}),
  logr:debug({sensor, sync, ok, "syncd. fanning out", [FanoutPids]}),
  [Pid ! {handle, {forward, self(), SensorVector}} || Pid <- FanoutPids],
  NewModState.