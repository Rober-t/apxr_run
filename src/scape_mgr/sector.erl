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
%%% @doc Sectors are the subcomponents/processes that make up a Scape. See the
%%%      Scape module for a description of what is a Scape.
%%% @end
%%%----------------------------------------------------------------------------
-module(sector).

-behaviour(gen_server).

%% Start/Stop
-export([
  start_link/2,
  stop/1
]).

%% API
-export([
  enter/3,
  sense/4,
  actuate/5,
  leave/3,
  remove/2,
  insert/3
]).

%% Term storage API
-export([
  store/2,
  fetch/1, fetch/2,
  delete/1, delete/2,
  update_counter/4
]).

%% gen_server callbacks
-export([
  init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2
]).

%% Xref
-ignore_xref([
  start_link/2,
  stop/1,
  behaviour_info/1,
  delete/1
]).

%%%============================================================================
%%% Types
%%%============================================================================

-record(sector_state, {
  mod_name :: atom(),
  mod_state :: any()
}).

-type state() :: #sector_state{}.
-type mod_name() :: {public, atom()}.
-type mod_state() :: any().
-type scape_id() :: atom() | float() | {float(), scape} | {atom(), scape}.

-export_type([
  mod_name/0,
  scape_id/0
]).

%%%============================================================================
%%% Behavior
%%%============================================================================

-callback init(Params :: any()) ->
  {ok, InitialModState :: mod_state()}.

-callback enter(AgentId :: models:agent_id(), Params :: any(), State :: mod_state()) ->
  {Result :: success | undefined, NewModState :: mod_state()}.

-callback sense(AgentId :: models:agent_id(), Params :: any(), SensorPid :: pid(),
  State :: mod_state()) -> {Result :: atom() | [float()], NewModState :: mod_state()}.

-callback actuate(AgentId :: models:agent_id(), Function :: atom(), Params :: any(),
  State :: mod_state()) -> {Result :: {[float()], integer() | atom()}, NewModState :: mod_state()}.

-callback leave(AgentId :: models:agent_id(), Params :: any(), State :: mod_state()) ->
  {ok, NewModState :: mod_state()}.

-callback remove(AgentId :: models:agent_id(), ModState :: mod_state()) ->
  {Result :: any(), NewModState :: mod_state()}.

-callback insert(AgentId :: models:agent_id(), Params :: any(), ModState :: mod_state()) ->
  {ok, NewModState :: mod_state()}.

-callback terminate(Reason :: atom(), ModState :: mod_state()) ->
  ok.

-optional_callbacks([
  remove/2,
  insert/3,
  terminate/2
]).

%%%============================================================================
%%% API
%%%============================================================================

%%-----------------------------------------------------------------------------
%% @doc The start_link function spawns the sector process.
%% @end
%%-----------------------------------------------------------------------------
-spec start_link(atom(), integer()) -> {ok, pid()}.
start_link(Mod, UId) ->
  gen_server:start_link({local, to_atom(UId)}, ?MODULE, [Mod], []).

%%-----------------------------------------------------------------------------
%% @doc Enter sector.
%% @end
%%-----------------------------------------------------------------------------
-spec enter(integer(), models:agent_id(), any()) -> ok.
enter(UId, AgentId, Params) ->
  gen_server:cast(to_atom(UId), {enter, AgentId, Params}).

%%-----------------------------------------------------------------------------
%% @doc Gather sensory inputs from the environment.
%% @end
%%-----------------------------------------------------------------------------
-spec sense(integer(), models:agent_id(), pid(), any()) -> ok.
sense(UId, AgentId, SensorPid, Params) ->
  gen_server:cast(to_atom(UId), {sense, AgentId, SensorPid, Params}).

%%-----------------------------------------------------------------------------
%% @doc Perform various sector functions e.g. move, push, etc. The sector
%%      API is problem dependent. This function provides an interface
%%      to call various functions defined by the sector in question.
%% @end
%%-----------------------------------------------------------------------------
-spec actuate(integer(), models:agent_id(), atom(), pid(), any()) -> ok.
actuate(UId, AgentId, Function, ActuatorPid, Params) ->
  gen_server:cast(to_atom(UId), {actuate, AgentId, Function, ActuatorPid, Params}).

%%-----------------------------------------------------------------------------
%% @doc Leave sector.
%% @end
%%-----------------------------------------------------------------------------
-spec leave(integer(), models:agent_id(), any()) -> ok.
leave(UId, AgentId, Params) ->
  gen_server:cast(to_atom(UId), {leave, AgentId, Params}).

%%-----------------------------------------------------------------------------
%% @doc Remove Agent from sector.
%% @end
%%-----------------------------------------------------------------------------
-spec remove(integer(), models:agent_id()) -> any().
remove(UId, AgentId) ->
  gen_server:call(to_atom(UId), {remove, AgentId}).

%%-----------------------------------------------------------------------------
%% @doc Insert Agent into sector.
%% @end
%%-----------------------------------------------------------------------------
-spec insert(integer(), models:agent_id(), any()) -> ok.
insert(UId, AgentId, Params) ->
  gen_server:call(to_atom(UId), {insert, AgentId, Params}).

%%-----------------------------------------------------------------------------
%% @doc Sends a signal to the Sector process requesting it to stop.
%% @end
%%-----------------------------------------------------------------------------
-spec stop(integer()) -> ok.
stop(UId) ->
  gen_server:cast(to_atom(UId), {stop, normal}).

%%%============================================================================
%%% Sector ETS helper functions.
%%%============================================================================

%%-----------------------------------------------------------------------------
%% @doc Insert object.
%% @end
%%-----------------------------------------------------------------------------
-spec store(t1|t2|t3|t4|t5|t6|t7|t8|t9|t10, term()) -> true.
store(Table, Value) ->
  true = shards:insert(Table, Value).

%%-----------------------------------------------------------------------------
%% @doc Lookup object.
%% @end
%%-----------------------------------------------------------------------------
-spec fetch(t1|t2|t3|t4|t5|t6|t7|t8|t9|t10, term()) -> list().
fetch(Table, Key) ->
  shards:lookup(Table, Key).

%%-----------------------------------------------------------------------------
%% @doc Return all objects.
%% @end
%%-----------------------------------------------------------------------------
-spec fetch(t1|t2|t3|t4|t5|t6|t7|t8|t9|t10) -> list().
fetch(Table) ->
  shards:match_object(Table, {'$0', '$1'}).

%%-----------------------------------------------------------------------------
%% @doc Delete object.
%% @end
%%-----------------------------------------------------------------------------
-spec delete(t1|t2|t3|t4|t5|t6|t7|t8|t9|t10, term()) -> true.
delete(Table, Key) ->
  true = shards:delete(Table, Key).

%%-----------------------------------------------------------------------------
%% @doc Delete entire table.
%% @end
%%-----------------------------------------------------------------------------
-spec delete(t1|t2|t3|t4|t5|t6|t7|t8|t9|t10) -> true.
delete(Table) ->
  true = shards:delete(Table).

%%-----------------------------------------------------------------------------
%% @doc Update counter.
%% @end
%%-----------------------------------------------------------------------------
-spec update_counter(t1|t2|t3|t4|t5|t6|t7|t8|t9|t10, term(), tuple(), tuple()) -> integer().
update_counter(Table, Key, UpdateOp, Default) ->
  shards:update_counter(Table, Key, UpdateOp, Default).

%%%============================================================================
%%% gen_server callbacks
%%%============================================================================

%%-----------------------------------------------------------------------------
%% @private
%% @doc Initializes the Sector server.
%% @end
%%-----------------------------------------------------------------------------
init([Mod]) ->
  process_flag(trap_exit, true),
  utils:random_seed(),
  M = utils:get_module(Mod),
  {ok, ModState} = M:init(Mod),
  logr:debug({sector, init, ok, undefined, [M]}),
  S = #sector_state{mod_name = M, mod_state = ModState},
  {ok, S}.

%%-----------------------------------------------------------------------------
%% @private
%% @doc Handle call messages.
%% @end
%%-----------------------------------------------------------------------------
handle_call({remove, AgentId}, _F, State) ->
  {Result, NewState} = do_remove(AgentId, State),
  {reply, Result, NewState};

handle_call({insert, AgentId, Params}, _F, State) ->
  {Result, NewState} = do_insert(AgentId, Params, State),
  {reply, Result, NewState};

handle_call(Request, From, State) ->
  logr:warning({sector, msg, error, "unexpected handle_call", [Request, From]}),
  {reply, ok, State}.

%%-----------------------------------------------------------------------------
%% @private
%% @doc Handle cast messages.
%% @end
%%-----------------------------------------------------------------------------
-spec handle_cast(
  {enter, models:agent_id(), _} |
  {leave, models:agent_id(), _} |
  {sense, models:agent_id(), pid(), any()} |
  {actuate, models:agent_id(), atom(), pid(), any()}, state()) -> {noreply, state()}.
handle_cast({enter, AgentId, Params}, S) ->
  NewState = do_enter(AgentId, Params, S),
  {noreply, NewState};

handle_cast({sense, AgentId, SensorPid, Params}, S) ->
  NewState = do_sense(AgentId, Params, SensorPid, S),
  {noreply, NewState};

handle_cast({actuate, AgentId, Function, ActuatorPid, Params}, S) ->
  NewState = do_actuate(AgentId, Function, ActuatorPid, Params, S),
  {noreply, NewState};

handle_cast({leave, AgentId, Params}, S) ->
  NewState = do_leave(AgentId, Params, S),
  {noreply, NewState};

handle_cast({stop, normal}, State) ->
  {stop, normal, State};

handle_cast({stop, shutdown}, State) ->
  {stop, shutdown, State}.

%%-----------------------------------------------------------------------------
%% @private
%% @doc Handle info messages.
%% @end
%%-----------------------------------------------------------------------------
-spec handle_info(any(), state()) -> {noreply, state()}.
handle_info(Info, State) ->
  case Info of
    {'EXIT', _Pid, normal} ->
      {noreply, State};
    {'EXIT', Pid, Reason} ->
      logr:debug({sector, msg, ok, "exit message", [Pid]}),
      {stop, Reason, State};
    UnexpectedMsg ->
      logr:warning({sector, msg, error, "unexpected info message", [UnexpectedMsg]}),
      {noreply, State}
  end.

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
  logr:debug({sector, terminate, ok, undefined, [Reason]}),
  Mod = State#sector_state.mod_name,
  case erlang:function_exported(Mod, terminate, 2) of
    true ->
      Mod:terminate(Reason, State#sector_state.mod_state);
    false ->
      ok
  end.

%%%============================================================================
%%% Internal functions
%%%============================================================================

to_atom(UId) when is_integer(UId) ->
  list_to_atom(integer_to_list(UId));
to_atom(UId) when is_list(UId) ->
  list_to_atom(UId);
to_atom(UId) when is_atom(UId) ->
  UId.

do_enter(AgentId, Params, S) ->
  Mod = S#sector_state.mod_name,
  {_Res, NewModS} = Mod:enter(AgentId, Params, S#sector_state.mod_state),
  S#sector_state{mod_state = NewModS}.

do_sense(AgentId, Params, SensorPid, S) ->
  Mod = S#sector_state.mod_name,
  {Result, NewModS} = Mod:sense(AgentId, Params, SensorPid, S#sector_state.mod_state),
  agent_mgr_client:percept(SensorPid, Result),
  S#sector_state{mod_state = NewModS}.

do_actuate(AgentId, Function, ActuatorPid, Params, S) ->
  Mod = S#sector_state.mod_name,
  {{Fitness, HaltFlag}, NewModS} = Mod:actuate(
    AgentId, Function, Params, S#sector_state.mod_state),
  agent_mgr_client:fitness(ActuatorPid, Fitness, HaltFlag),
  S#sector_state{mod_state = NewModS}.

do_leave(AgentId, Params, S) ->
  Mod = S#sector_state.mod_name,
  {ok, NewModS} = Mod:leave(AgentId, Params, S#sector_state.mod_state),
  S#sector_state{mod_state = NewModS}.

do_remove(AgentId, S) ->
  Mod = S#sector_state.mod_name,
  {Params, NewModS} = case erlang:function_exported(Mod, remove, 2) of
    true ->
      Mod:remove(AgentId, S#sector_state.mod_state);
    false ->
      {ok, S#sector_state.mod_state}
  end,
  {Params, S#sector_state{mod_state = NewModS}}.

do_insert(AgentId, Params, S) ->
  Mod = S#sector_state.mod_name,
  {ok, NewModS} = case erlang:function_exported(Mod, insert, 3) of
    true ->
      Mod:insert(AgentId, Params, S#sector_state.mod_state);
    false ->
      {ok, S#sector_state.mod_state}
  end,
  {ok, S#sector_state{mod_state = NewModS}}.