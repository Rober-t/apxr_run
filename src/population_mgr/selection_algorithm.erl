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
%%% @doc The selection_algorithm module is a container for the
%%%      selection_algorithm functions. By keeping all the selection
%%%      functions in this module, it makes it easier for us to later
%%%      add new ones, and then simply reference them by their name.
%%% @end
%%%----------------------------------------------------------------------------
-module(selection_algorithm).

%% API
-export([
  hof_competition/3,
  hof_rank/3,
  hof_top3/3,
  hof_efficiency/3,
  hof_random/3,
  choose_winners/6
]).

%% Xref
-ignore_xref([
  hof_competition/3,
  hof_rank/3,
  hof_top3/3,
  hof_efficiency/3,
  hof_random/3
]).

%%%============================================================================
%%% API
%%%============================================================================

%%-----------------------------------------------------------------------------
%% @doc hof_competition.
%% @end
%%-----------------------------------------------------------------------------
-spec hof_competition(models:specie_id(), [models:champion()], non_neg_integer()) -> ok.
hof_competition(SpecieId, RemainingChampionDesignators, SpecieSizeLimit) ->
  S = db:read(SpecieId, specie),
  SHOF = models:get(hall_of_fame, S),
  SHOFRatio = app_config:get_env(shof_ratio),
  EFF = app_config:get_env(selection_algorithm_efficiency),
  NewGenIds = case SHOFRatio < 1 of
    true ->
      Actives = RemainingChampionDesignators,
      SHOFFitnessScaled = fitness_scaled(SHOF, EFF),
      ActiveFitnessScaled = fitness_scaled(Actives, EFF),
      TotFitnessActives = lists:sum([MainFitness || {MainFitness, _Id} <- ActiveFitnessScaled]),
      TotFitnessSHOFs = lists:sum([MainFitness || {MainFitness, _Id} <- SHOFFitnessScaled]),
      next_generation(SpecieId, ActiveFitnessScaled, SHOFFitnessScaled, TotFitnessActives,
        TotFitnessSHOFs, SHOFRatio, SpecieSizeLimit);
    false ->
      Allotments = fitness_scaled(SHOF, EFF),
      Tot = lists:sum([MainFitness || {MainFitness, _Id} <- Allotments]),
      choose_winners(SpecieId, Allotments, Tot, [], [], SpecieSizeLimit)
  end,
  [ets:insert(active_agents, {UId, agent, SpecieId}) || {agent, UId} <- NewGenIds],
  ok.

%%%-----------------------------------------------------------------------------
%% @doc hof_rank returns a list of new generation of agents for a single specie.
%% @end
%%-----------------------------------------------------------------------------
-spec hof_rank(models:specie_id(), [models:agent_id()], non_neg_integer()) -> ok.
hof_rank(SpecieId, RemainingChampionDesignators, SpecieSizeLimit) ->
  S = db:read(SpecieId, specie),
  db:write(models:set({agent_ids, []}, S), specie),
  SHOF = models:get(hall_of_fame, S),
  SHOFRatio = app_config:get_env(shof_ratio),
  NewGenIds = case SHOFRatio < 1 of
    true ->
      Actives = RemainingChampionDesignators,
      ActivesRanked = rank(Actives),
      SHOFRanked = rank(SHOF),
      TotFitnessActives = lists:sum([MainFitness || {MainFitness, _Id} <- ActivesRanked]),
      TotFitnessSHOFs = lists:sum([MainFitness || {MainFitness, _Id} <- SHOFRanked]),
      next_generation(SpecieId, ActivesRanked, SHOFRanked, TotFitnessActives,
        TotFitnessSHOFs, SHOFRatio, SpecieSizeLimit);
    false ->
      SHOF = models:get(hall_of_fame, S),
      Allotments = rank(SHOF),
      Tot = lists:sum([Val || {Val, _Id} <- Allotments]),
      choose_winners(SpecieId, Allotments, Tot, [], [], SpecieSizeLimit)
  end,
  [ets:insert(active_agents, {UId, agent, SpecieId}) || {agent, UId} <- NewGenIds],
  ok.

%%-----------------------------------------------------------------------------
%% @doc hof_top3.
%% @end
%%-----------------------------------------------------------------------------
-spec hof_top3(models:specie_id(), [models:agent_id()], non_neg_integer()) -> ok.
hof_top3(SpecieId, _RemainingChampionDesignators, SpecieSizeLimit) ->
  S = db:read(SpecieId, specie),
  db:write(models:set({agent_ids, []}, S), specie),
  SHOF = models:get(hall_of_fame, S),
  Allotments = lists:sublist(lists:reverse(sort_champs(SHOF)), 3),
  Tot = lists:sum([Val || {Val, _Id} <- Allotments]),
  NewGenIds = choose_winners(SpecieId, Allotments, Tot, [], [], SpecieSizeLimit),
  [ets:insert(active_agents, {UId, agent, SpecieId}) || {agent, UId} <- NewGenIds],
  ok.

%%-----------------------------------------------------------------------------
%% @doc hof_efficiency.
%% @end
%%-----------------------------------------------------------------------------
-spec hof_efficiency(models:specie_id(), [models:agent_id()], non_neg_integer()) -> ok.
hof_efficiency(SpecieId, RemainingChampionDesignators, SpecieSizeLimit) ->
  S = db:read(SpecieId, specie),
  db:write(models:set({agent_ids, []}, S), specie),
  SHOF = models:get(hall_of_fame, S),
  SHOFRatio = app_config:get_env(shof_ratio),
  NewGenIds = case SHOFRatio < 1 of
    true ->
      Actives = RemainingChampionDesignators,
      ActiveNeuralEffScaled = neural_eff_scaled(Actives),
      SHOFNeuralEffScaled = neural_eff_scaled(SHOF),
      TotFitnessActives = lists:sum([MainFitness || {MainFitness, _Id} <- ActiveNeuralEffScaled]),
      TotFitnessSHOFs = lists:sum([MainFitness || {MainFitness, _Id} <- SHOFNeuralEffScaled]),
      next_generation(SpecieId, ActiveNeuralEffScaled, SHOFNeuralEffScaled,
        TotFitnessActives, TotFitnessSHOFs, SHOFRatio, SpecieSizeLimit);
    false ->
      SHOFNeuralEffScaled = neural_eff_scaled(SHOF),
      TotFitnessSHOFs = lists:sum([MainFitness || {MainFitness, _Id} <- SHOFNeuralEffScaled]),
      choose_winners(SpecieId, SHOFNeuralEffScaled, TotFitnessSHOFs, [], [], SpecieSizeLimit)
  end,
  [ets:insert(active_agents, {UId, agent, SpecieId}) || {agent, UId} <- NewGenIds],
  ok.

%%-----------------------------------------------------------------------------
%% @doc hof_random.
%% @end
%%-----------------------------------------------------------------------------
-spec hof_random(models:specie_id(), [models:agent_id()], non_neg_integer()) -> ok.
hof_random(SpecieId, RemainingChampionDesignators, SpecieSizeLimit) ->
  S = db:read(SpecieId, specie),
  db:write(models:set({agent_ids, []}, S), specie),
  SHOF = models:get(hall_of_fame, S),
  SHOFRatio = app_config:get_env(shof_ratio),
  NewGenIds = case SHOFRatio < 1 of
    true ->
      Actives = RemainingChampionDesignators,
      ActiveRandomScaled = random_scaled(Actives),
      SHOFRandomScaled = random_scaled(SHOF),
      TotFitnessActives = lists:sum([MainFitness || {MainFitness, _Id} <-  ActiveRandomScaled]),
      TotFitnessSHOFs = lists:sum([MainFitness || {MainFitness, _Id} <- SHOFRandomScaled]),
      next_generation(SpecieId, ActiveRandomScaled, SHOFRandomScaled, TotFitnessActives,
        TotFitnessSHOFs, SHOFRatio, SpecieSizeLimit);
    false ->
      SHOF = models:get(hall_of_fame, S),
      SHOFRandomScaled = random_scaled(SHOF),
      TotFitnessSHOFs = lists:sum([MainFitness || {MainFitness, _Id} <- SHOFRandomScaled]),
      choose_winners(SpecieId, SHOFRandomScaled, TotFitnessSHOFs, [], [], SpecieSizeLimit)
  end,
  [ets:insert(active_agents, {UId, agent, SpecieId}) || {agent, UId} <- NewGenIds],
  ok.

%%-----------------------------------------------------------------------------
%% @doc choose_winners.
%% @end
%%-----------------------------------------------------------------------------
-spec choose_winners(models:specie_id(), [models:agent_id()], float(), [models:agent_id()],
  [models:agent_id()], non_neg_integer()) -> [models:agent_id()].
choose_winners(SpecieId, _Agents, _TotalFitness, OffspringAcc, ReentryAcc, 0) ->
  reenter(ReentryAcc, SpecieId),
  OffspringAcc ++ ReentryAcc;
choose_winners(SpecieId, Agents, TotalFitness, OffspringAcc, ReentryAcc, AgentIndex) ->
  try choose_winner(SpecieId, Agents, (rand:uniform(100) / 100) * TotalFitness, 0) of
    {OffspringId, offspring} ->
      choose_winners(SpecieId, Agents, TotalFitness, [OffspringId | OffspringAcc], ReentryAcc,
        AgentIndex -1);
    {AgentId, reentry} ->
      case lists:member(AgentId, ReentryAcc) of
        true ->
          choose_winners(SpecieId, Agents, TotalFitness, OffspringAcc, ReentryAcc, AgentIndex);
        false ->
          choose_winners(SpecieId, Agents, TotalFitness, OffspringAcc,
            lists:append(AgentId, ReentryAcc), AgentIndex -1)
      end
  catch
    C:R:_Stacktrace ->
      logr:error({selection_algorithm, choose_winners, error,
        "choose winner crashing", [C, R]}),
      choose_winners(SpecieId, Agents, TotalFitness, OffspringAcc, ReentryAcc, AgentIndex)
  end.

%%%============================================================================
%%% Internal functions
%%%============================================================================

reenter([AgentId | ReentryIds], SpecieId) ->
  logr:debug({selection_algorithm, reenter, ok, undefined, [AgentId]}),
  S = db:read(SpecieId, specie),
  SHOF = models:get(hall_of_fame, S),
  USHOF = lists:keydelete(AgentId, 3, SHOF),
  US = models:set({hall_of_fame, USHOF}, S),
  A = db:read(AgentId, agent),
  UA = models:set({champion_flag, [reentered | models:get(champion_flag, A)]}, A),
  db:write(US, specie),
  db:write(UA, agent),
  reenter(ReentryIds, SpecieId);
reenter([], _SpecieId) ->
  ok.

choose_winner(_SpecieId, [{_PortionSize, AgentId}], _Index, _Acc) ->
  ReEntryProbability = app_config:get_env(re_entry_probability),
  new_winner(ReEntryProbability, AgentId);

choose_winner(SpecieId, [{PortionSize, AgentId} | Allotments], Index, Acc) ->
  ReEntryProbability = app_config:get_env(re_entry_probability),
  case (Index >= Acc) and (Index =< (Acc + PortionSize)) of
    true ->
      new_winner(ReEntryProbability, AgentId);
    false ->
      choose_winner(SpecieId, Allotments, Index, Acc + PortionSize)
  end.

%%-----------------------------------------------------------------------------
%% @private
%% @doc The create_mutant_agent_copy first creates a clone of the AgentId,
%%      and then uses the genome_mutator:mutate/1 function to mutate that
%%      clone, returning the id of the cloned agent to the caller.
%% @end
%%----------------------------------------------------------------------------
create_mutant_agent_copy(AgentId) ->
  AgentCloneId = genotype:clone_agent(AgentId),
  genome_mutator:mutate(AgentCloneId),
  AgentCloneId.

assign_rank([{_MainFitness, AgentId} | Champions], [Rank | RankList], Acc) ->
    assign_rank(Champions, RankList, [{Rank, AgentId} | Acc]);
assign_rank([], [], Acc) ->
  Acc.

fitness_scaled(Champs, EFF) ->
  [calc_fitness_scaled(C, EFF) || C <- Champs].

calc_fitness_scaled(C, EFF) ->
  {
    models:get(fs, C) * (models:get(main_fitness, C) / math:pow(models:get(tot_n, C), EFF)),
    models:get(id, C)
  }.

next_generation(SpecieId, ActiveFitnessScaled, SHOFFitnessScaled, TotFitnessActives,
  TotFitnessSHOFs, SHOFRatio, SpecieSizeLimit) ->
    choose_winners(SpecieId, ActiveFitnessScaled, TotFitnessActives, [], [],
      round((1 - SHOFRatio) * SpecieSizeLimit)),
    choose_winners(SpecieId, SHOFFitnessScaled, TotFitnessSHOFs, [], [],
      round(SHOFRatio * SpecieSizeLimit)).

rank(Champs) ->
  assign_rank(sort_champs(Champs), lists:seq(1, length(Champs)), []).

sort_champs(Champs) ->
  lists:sort([{models:get(fs, C) * models:get(main_fitness, C), models:get(id, C)} || C <- Champs]).

neural_eff_scaled(Champs) ->
  [{models:get(fs, C) * models:get(main_fitness, C) / models:get(tot_n, C), models:get(id, C)} ||
  C <- Champs].

new_winner(ReEntryProbability, AgentId) ->
  case rand:uniform() =< ReEntryProbability of
    true ->
      {AgentId, reentry};
    false ->
      A = db:read(AgentId, agent),
      OffspringAgentId = create_mutant_agent_copy(AgentId),
      UA = models:set({offspring_ids, [OffspringAgentId | models:get(offspring_ids, A)]}, A),
      db:write(UA, agent),
      OffspringA = db:read(OffspringAgentId, agent),
      UOffspringA = models:set({champion_flag, [false | models:get(champion_flag, OffspringA)]},
        OffspringA),
      db:write(UOffspringA, agent),
      {OffspringAgentId, offspring}
  end.

random_scaled(Champs) ->
  [{models:get(fs, C) * 1, models:get(id, C)} || C <- Champs].