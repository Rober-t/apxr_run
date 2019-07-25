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
%%% @doc Defines generic private scape behavior.
%%%      Scapes are self contained simulated worlds or virtual environments,
%%%      that is, they are not necessarily physical. They can be thought of as
%%%      a way of interfacing with the problem in question. Scapes are composed
%%%      of two parts, a simulation of an environment or a problem we are
%%%      applying the NN to, and a function that can keep track of the NN’s
%%%      performance. Scapes run outside the NN systems, as independent
%%%      processes with which the NNs interact using their sensors and
%%%      actuators. There are two types of scapes. One type of scape, private,
%%%      is spawned for each NN during the NN’s creation, and destroyed when
%%%      that NN is taken offline. Another type of scape, public, is
%%%      persistent, they exist regardless of the NNs, and allow multiple NNs
%%%      to interact with them at the same time, and thus they can allow those
%%%      NNs to interact with each other too. This module defines the private
%%%      scape.
%%% @end
%%%----------------------------------------------------------------------------
-module(private_scape).

-behaviour(gen_server).

%% Start/Stop
-export([
  start_link/2
]).

%% API
-export([
  sense/4,
  actuate/5
]).

%% gen_server callbacks
-export([
  init/1,
  handle_call/3,
  handle_cast/2,
  terminate/2
]).

%% Xref
-ignore_xref([
  start_link/2,
  behaviour_info/1
]).

%%%============================================================================
%%% Types
%%%============================================================================

-record(priv_scape_state, {
  agent_id :: models:agent_id(),
  mod_name :: atom(),
  mod_state :: any()
}).

-type state() :: #priv_scape_state{}.
-type mod_state() :: any().

%%%============================================================================
%%% Behavior
%%%============================================================================

-callback init(Params :: any()) ->
  {ok, InitialModState :: mod_state()}.

-callback sense(Params :: any(), State :: mod_state()) ->
  {Result :: atom() | [float()], NewModState :: mod_state()}.

-callback actuate(Function :: atom(), Params :: any(), AgentId :: models:agent_id(),
  State :: mod_state())
-> {Result :: {[float()], integer() | atom()}, NewModState :: mod_state()}.

-callback terminate(Reason :: atom(), ModState :: mod_state()) ->
  ok.

-optional_callbacks([
  terminate/2
]).

%%%============================================================================
%%% API
%%%============================================================================

%%-----------------------------------------------------------------------------
%% @doc The start_link function spawns the PrivateScape process.
%% @end
%%-----------------------------------------------------------------------------
-spec start_link(models:agent_id(), atom()) -> {ok, pid()}.
start_link(AgentId, Mod) ->
  gen_server:start_link(?MODULE, {AgentId, Mod}, []).

%%-----------------------------------------------------------------------------
%% @doc Gathers sensory inputs from the environment.
%% @end
%%-----------------------------------------------------------------------------
-spec sense(pid(), models:agent_id(), pid(), any()) -> ok.
sense(Pid, AgentId, SensorPid, Params) ->
  gen_server:cast(Pid, {sense, AgentId, SensorPid, Params}).

%%-----------------------------------------------------------------------------
%% @doc Performs various PrivateScape functions e.g. move, push, etc. The scape
%%      API is problem dependent. This function provides an interface
%%      to call various functions defined by the PrivateScape in question.
%% @end
%%-----------------------------------------------------------------------------
-spec actuate(pid(), models:agent_id(), pid(), atom(), any()) -> ok.
actuate(Pid, AgentId, ActuatorPid, Function, Params) ->
  gen_server:cast(Pid, {actuate, AgentId, ActuatorPid, Function, Params}).

%%%============================================================================
%%% gen_server callbacks
%%%============================================================================

%%-----------------------------------------------------------------------------
%% @private
%% @doc Initializes the PrivateScape server.
%% @end
%%-----------------------------------------------------------------------------
-spec init({models:agent_id(), atom()}) -> {ok, state()}.
init({AgentId, Mod}) ->
  process_flag(trap_exit, true),
  utils:random_seed(),
  M = utils:get_module(Mod),
  {ok, ModState} = M:init([]),
  logr:debug({private_scape, init, ok, undefined, [M]}),
  S = #priv_scape_state{agent_id = AgentId, mod_name = M, mod_state = ModState},
  {ok, S}.

%%-----------------------------------------------------------------------------
%% @private
%% @doc Handle call messages.
%% @end
%%-----------------------------------------------------------------------------
-spec handle_call(term(), {pid(), term()}, state()) -> {reply, ok, state()}.
handle_call(_Request, _From, State) ->
  logr:warning({private_scape, msg, error, "unexpected handle_call", []}),
  {reply, ok, State}.

%%-----------------------------------------------------------------------------
%% @private
%% @doc Handle cast messages.
%% @end
%%-----------------------------------------------------------------------------
-spec handle_cast(any(), state()) -> {noreply, state()}.
handle_cast({sense, _AgentId, SensorPid, Params}, S) ->
  NewState = do_sense(Params, SensorPid, S),
  {noreply, NewState};

handle_cast({actuate, AgentId, ActuatorPid, Function, Params}, S) ->
  NewState = do_actuate(Function, Params, AgentId, ActuatorPid, S),
  {noreply, NewState}.

%%-----------------------------------------------------------------------------
%% @private
%% @doc This function is called by a gen_server when it is about to terminate.
%%      It should be the opposite of Module:init/1 and do any necessary
%%      cleaning up. When it returns, the gen_server terminates with Reason.
%%      The return value is ignored.
%% @end
%%-----------------------------------------------------------------------------
-spec terminate(any(), state()) -> ok.
terminate(Reason, State) ->
  logr:debug({private_scape, terminate, ok, undefined, [Reason]}),
  Mod = State#priv_scape_state.mod_name,
  case erlang:function_exported(Mod, terminate, 2) of
    true ->
      Mod:terminate(Reason, State#priv_scape_state.mod_state);
    false ->
      ok
  end.

%%%============================================================================
%%% Internal functions
%%%============================================================================

do_sense(Params, SensorPid, S) ->
  Mod = S#priv_scape_state.mod_name,
  {Result, NewModState} = Mod:sense(Params, S#priv_scape_state.mod_state),
  sensor:percept(SensorPid, Result),
  S#priv_scape_state{mod_state = NewModState}.

do_actuate(Function, Params, AgentId, ActuatorPid, S) ->
  Mod = S#priv_scape_state.mod_name,
  {Result, NewModState} = Mod:actuate(Function, Params, AgentId, S#priv_scape_state.mod_state),
  actuator:fitness(ActuatorPid, Result),
  S#priv_scape_state{mod_state = NewModState}.