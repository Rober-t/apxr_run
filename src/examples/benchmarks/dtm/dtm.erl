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
%%% @doc Discrete T-maze scape for benchmarking purposes.
%%% @end
%%%----------------------------------------------------------------------------
-module(dtm).

-behaviour(private_scape).

%% API
-export([]).

%% Scape callbacks
-export([
  init/1,
  sense/2,
  actuate/4
]).

%%%============================================================================
%%% Types
%%%============================================================================

-record(dtm_sector, {
  id :: [integer()],
  description = [] :: [{integer(), [integer()], [integer()]}],
  r :: float()
}).

-record(dtm_state, {
  agent_position = [0, 0] :: [integer()],
  agent_direction = 90 :: integer(),
  sectors = [] :: [#dtm_sector{}],
  total_runs = 100 :: integer(),
  run_index = 0 :: integer(),
  switch_event :: integer(),
  switched = false :: boolean(),
  step_index = 0 :: integer(),
  fitness_acc = 50.0 :: float()
}).

-type state() :: #dtm_state{}.

%%%============================================================================
%%% Scape callbacks
%%%============================================================================

%%%----------------------------------------------------------------------------
%%% @private
%%% @doc Initializes the scape process.
%%% @end
%%%----------------------------------------------------------------------------
-spec init([]) -> {ok, state()}.
init([]) ->
  NewState = dtm_sim(),
  {ok, NewState}.

%%%----------------------------------------------------------------------------
%%% @private
%%% @doc Returns sensory input.
%%% @end
%%%----------------------------------------------------------------------------
-spec sense([atom()], state() | undefined) -> {[float()], state()}.
sense(Params, S) ->
  APos = S#dtm_state.agent_position,
  ADir = S#dtm_state.agent_direction,
  Sector = lists:keyfind(APos, 2, S#dtm_state.sectors),
  {ADir, _NextSec, RangeSense} = lists:keyfind(ADir, 1, Sector#dtm_sector.description),
  SenseSignal = case Params of
    [all] ->
      RangeSense ++ [Sector#dtm_sector.r];
    [range_sense]->
      RangeSense;
    [reward] ->
      [Sector#dtm_sector.r]
  end,
  {SenseSignal, maybe_switch(S)}.

%%%----------------------------------------------------------------------------
%%% @private
%%% @doc Performs output action.
%%% @end
%%%----------------------------------------------------------------------------
-spec actuate(atom(), {[float()], [integer()]}, models:agent_id(), state()) ->
{{[float()], integer()}, state()}.
actuate(move, {_Params, [Move]}, AgentId, S) ->
  APos = S#dtm_state.agent_position,
  ADir = S#dtm_state.agent_direction,
  Sector = lists:keyfind(APos, 2, S#dtm_state.sectors),
  UStepIndex = S#dtm_state.step_index + 1,
  {ADir, NextSec, _RangeSense} = lists:keyfind(ADir, 1, Sector#dtm_sector.description),
  if
    (APos == [1, 1]) or (APos == [-1, 1]) ->
      UpdatedRunIndex = S#dtm_state.run_index + 1,
      case UpdatedRunIndex >= S#dtm_state.total_runs of
        true ->
          US = S#dtm_state{
            switch_event = 35 + rand:uniform(30),
            sectors = set_tmaze_sectors(),
            switched = false,
            agent_position = [0, 0],
            agent_direction = 90,
            run_index = 0,
            step_index = 0,
            fitness_acc = 50.0
          },
          {{[S#dtm_state.fitness_acc + Sector#dtm_sector.r], 1}, maybe_switch(US)};
        false ->
          US = S#dtm_state{
            agent_position = [0, 0],
            agent_direction = 90,
            run_index = UpdatedRunIndex,
            step_index = 0,
            fitness_acc = S#dtm_state.fitness_acc + Sector#dtm_sector.r
          },
          {{[0.0], 0}, maybe_switch(US)}
      end;
    Move > 0.33 -> % clockwise
      NewDir = (S#dtm_state.agent_direction + 270) rem 360,
      {NewDir, NewNextSec, _NewRangeSense} = lists:keyfind(NewDir, 1,
        Sector#dtm_sector.description),
      US = S#dtm_state{agent_direction = NewDir},
      move(AgentId, US, NewNextSec, UStepIndex);
    Move < -0.33 -> % counterclockwise
      NewDir = (S#dtm_state.agent_direction + 90) rem 360,
      {NewDir, NewNextSec, _NewRangeSense} = lists:keyfind(NewDir, 1,
        Sector#dtm_sector.description),
      US = S#dtm_state{agent_direction = NewDir},
      move(AgentId, US, NewNextSec, UStepIndex);
    true -> % forward
      move(AgentId, S, NextSec, UStepIndex)
  end.

%%%============================================================================
%%% Internal functions
%%%============================================================================

dtm_sim() ->
  utils:random_seed(),
  #dtm_state{switch_event = 35 + rand:uniform(30), sectors = set_tmaze_sectors()}.

move(_AgentId, S, NextSec, UStepIndex) ->
  case NextSec of
    [] -> % wall crash/restart_state
      UpdatedRunIndex = S#dtm_state.run_index + 1,
      case UpdatedRunIndex >= S#dtm_state.total_runs of
        true ->
          US = S#dtm_state{
            switch_event = 35 + rand:uniform(30),
            sectors = set_tmaze_sectors(),
            switched = false,
            run_index = 0,
            step_index = 0,
            agent_position = [0, 0],
            agent_direction = 90,
            fitness_acc = 50.0
          },
          {{[US#dtm_state.fitness_acc - 0.4], 1}, maybe_switch(US)};
        false ->
          US = S#dtm_state{
            agent_position = [0, 0],
            agent_direction = 90,
            run_index = UpdatedRunIndex,
            step_index = 0,
            fitness_acc = S#dtm_state.fitness_acc - 0.4
          },
          {{[0.0], 0}, maybe_switch(US)}
        end;
    _ -> % move
      US = S#dtm_state{agent_position = NextSec, step_index = UStepIndex},
      {{[0.0], 0}, maybe_switch(US)}
  end.

maybe_switch(S) ->
  case (S#dtm_state.run_index == S#dtm_state.switch_event) and (S#dtm_state.switched == false) of
    true ->
      Sectors = S#dtm_state.sectors,
      SA = lists:keyfind([1, 1], 2, Sectors),
      SB = lists:keyfind([-1, 1], 2, Sectors),
      USA = SA#dtm_sector{r = SB#dtm_sector.r},
      USB = SB#dtm_sector{r = SA#dtm_sector.r},
      USectors = lists:keyreplace([-1, 1], 2, lists:keyreplace([1, 1], 2, Sectors, USA), USB),
      US = S#dtm_state{sectors = USectors, switched = true},
      US;
    false ->
      S
  end.

set_tmaze_sectors() ->
  [
    #dtm_sector{
      id = [0,0],
      description = [{0,[],[1,0,0]},{90,[0,1],[0,1,0]},{180,[],[0,0,1]},{270,[],[0,0,0]}],
      r = 0.0
    },
    #dtm_sector{
      id = [0,1],
      description = [{0,[1,1],[0,1,1]},{90,[],[1,0,1]},{180,[-1,1],[1,1,0]},{270,[0,0],[1,1,1]}],
      r = 0.0
    },
    #dtm_sector{
      id = [1,1],
      description = [{0,[],[0,0,0]},{90,[],[2,0,0]},{180,[0,1],[0,2,0]},{270,[],[0,0,2]}],
      r = 1.0
    },
    #dtm_sector{
      id = [-1,1],
      description = [{0,[0,1],[0,2,0]},{90,[],[0,0,2]},{180,[],[0,0,0]},{270,[],[2,0,0]}],
      r = 0.2
    }
  ].