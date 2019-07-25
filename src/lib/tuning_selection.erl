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
%%% @doc The tuning_selection module contains all the tuning selection
%%%      functions, which accept as input four parameters:
%%%      1. All NIds belonging to the NN.
%%%      2. The agent’s generation, which is the number of topological
%%%      mutation phases that it has undergone.
%%%      3. The perturbation range, the multiplier of math:pi(), which when
%%%      used produces the spread value.
%%%      4. The annealing parameter, which is used to indicate how the
%%%      perturbation range decays with the age of the neuron to which synaptic
%%%      weight perturbation is applied. It makes less sense to perturb the
%%%      more stable elements of the NN system, less so than those elements
%%%      which have just recently been added to the NN system, and which still
%%%      need to be tuned and modified to work well with the already existing
%%%      larger system. The concept is that of simulated annealing. We
%%%      gather all these selection functions in their own module because there
%%%      are many ways to select neurons which should be perturbed in local
%%%      search during the tuning phase. This makes it easier for us to add new
%%%      selection functions later on, and see if a new function can improve
%%%      the performance.
%%%      The tuning selection function must not only select the neuron ids for
%%%      synaptic perturbation, but also compute the perturbation intensity,
%%%      the available range of the perturbation intensity, from which the
%%%      neuron will then randomly generate a weight perturbation value. Thus,
%%%      the selection_function creates a list of tuples rather than simply a
%%%      list of neuron ids. The selection_function outputs a list of the
%%%      following form: [{NId, Spread},...], where NId is the neuron id, and
%%%      Spread is the spread above and below 0, the value within which the
%%%      neuron generates the actual perturbation. The Spread equals the
%%%      perturbation_range value if there is no annealing, if annealing is
%%%      present (annealing_parameter =< 1), then the Spread is further
%%%      modified. The annealing factor must scale the Spread, proportional to
%%%      the age of the neuron whose synaptic weights are to be perturbed. In
%%%      tuning selection algorithms, the spread value is calculated as follows:
%%%      Spread = PerurbationRange * math:pi() * math:pow(AnnealingParam, NAge).
%%%      When AnnealingParameter = 1, there is no annealing. But when the
%%%      AnnealingParameter is set to a number lower than 1, then annealing is
%%%      exponentially proportional to the neuron's age.
%%% @end
%%%----------------------------------------------------------------------------
-module(tuning_selection).

%% API
-export([
  dynamic/4,
  dynamic_random/4,
  active/4,
  active_random/4,
  current/4,
  current_random/4,
  all/4,
  all_random/4
]).

%% Xref
-ignore_xref([
  dynamic/4,
  dynamic_random/4,
  active/4,
  active_random/4,
  current/4,
  current_random/4,
  all/4,
  all_random/4
]).

%%%============================================================================
%%% API
%%%============================================================================

%%-----------------------------------------------------------------------------
%% @doc The dynamic selection function randomly selects an age limit for
%%      its neuron id pool. The age limit is chosen by executing
%%      math:sqrt(1/rand:uniform()), which creates a value between 1 and
%%      infinity. Using this function there is 75% that the number will be
%%      =< 2, 25% that it will be >= 2, 11% that it will be >= 3...Every time
%%      this selection function is executed, the AgeLimit is generated anew,
%%      thus different times it will produce different neuron id pools for
%%      tuning.
%% @end
%%-----------------------------------------------------------------------------
-spec dynamic([{actuator | neuron, {float()}}], integer(), float(), float())
-> [{models:neuron_id(), float()}].
dynamic(Ids, AgentGeneration, PerturbationRange, AnnealingParameter) ->
  chosen_idps(Ids, AgentGeneration, PerturbationRange, AnnealingParameter).

%%-----------------------------------------------------------------------------
%% @doc dyanimic_random selection function composes the neuron id pool the
%%      same way as the dynamic/4 selection function, but after this id pool
%%      is generated, this selection function extracts ids from it randomly
%%      with a probability of 1/math:sqrt(Tot_Neurons). Thus the probability
%%      of a neuron being selected from this pool is proportional to the
%%      number of ids in that pool. If through chance no ids are selected,
%%      then the first element in the id pool is automatically selected, and
%%      given the highest spread.
%% @end
%%-----------------------------------------------------------------------------
-spec dynamic_random([{actuator | neuron, {float()}}], integer(), float(), float())
-> [{models:neuron_id(), float()}].
dynamic_random(Ids, AgentGeneration, PerturbationRange, AnnealingParameter) ->
  ChosenIdPs = chosen_idps(Ids, AgentGeneration, PerturbationRange, AnnealingParameter),
  MutationP = 1 / math:sqrt(length(ChosenIdPs)),
  choose_random_idps(MutationP, ChosenIdPs).

%%-----------------------------------------------------------------------------
%% @doc active selection algorithm composes a neuron id pool from all
%%      neurons who are younger than 3 generations.
%% @end
%%-----------------------------------------------------------------------------
-spec active([{actuator | neuron, {float()}}], integer(), float(), float())
-> [{models:neuron_id(), float()}].
active(Ids, AgentGeneration, PerturbationRange, AnnealingParameter) ->
  extract_cur_gen_idps(Ids, AgentGeneration, 3, PerturbationRange, AnnealingParameter, []).

%%-----------------------------------------------------------------------------
%% @doc active_random is a selection algorithm that composes an id pool by
%%      first creating a list of all neurons who are younger than 3
%%      generations, and then composing a sub list from it by randomly
%%      choosing elements from this list with a probability of
%%      1/math:sqrt(Tot_Neurons).
%% @end
%%-----------------------------------------------------------------------------
-spec active_random([{actuator | neuron, {float()}}], integer(), float(), float())
-> [{models:neuron_id(), float()}].
active_random(Ids, AGeneration, PerturbationR, AnnealingParam) ->
  ChosenIdPs = case extract_cur_gen_idps(Ids, AGeneration, 3, PerturbationR, AnnealingParam, []) of
    [] ->
      [Id | _] = Ids,
      [{Id, PerturbationR * math:pi()}];
    ExtractedIdPs ->
      ExtractedIdPs
  end,
  MutationP = 1 / math:sqrt(length(ChosenIdPs)),
  choose_random_idps(MutationP, ChosenIdPs).

%%-----------------------------------------------------------------------------
%% @doc current is a selection algorithm that returns a list of all neurons
%%      which have been added to the NN, or affected by mutation, during the
%%      last generation.
%% @end
%%-----------------------------------------------------------------------------
-spec current([{actuator | neuron, {float()}}], integer(), float(), float())
-> [{models:neuron_id(), float()}].
current(Ids, AgentGeneration, PerturbationRange, AnnealingParameter) ->
  case extract_cur_gen_idps(Ids, AgentGeneration, 0, PerturbationRange, AnnealingParameter, []) of
    [] ->
      [Id | _] = Ids,
      [{Id, PerturbationRange * math:pi()}];
    IdPs ->
      IdPs
  end.

%%-----------------------------------------------------------------------------
%% @doc current_random composes the list of tuples in the same way as
%%      current does, but then composes a sublist by randomly selecting
%%      elements from that list with a probability of
%%      1/math:sqrt(Tot_Neurons), and returning that to the caller.
%% @end
%%-----------------------------------------------------------------------------
-spec current_random([{actuator | neuron, {float()}}], integer(), float(), float())
-> [{models:neuron_id(), float()}].
current_random(Ids, AgentGeneration, PerturbationRange, AnnealingParameter) ->
  ChosenIdPs = current(Ids, AgentGeneration, PerturbationRange, AnnealingParameter),
  MutationP = 1 / math:sqrt(length(ChosenIdPs)),
  choose_random_idps(MutationP, ChosenIdPs).

%%-----------------------------------------------------------------------------
%% @doc all returns a list of tuples composed of all ids (and their spread
%%      values) belonging to the NN, to the caller.
%% @end
%%-----------------------------------------------------------------------------
-spec all([{actuator | neuron, {float()}}], integer(), float(), float())
-> [{models:neuron_id(), float()}].
all(Ids, AgentGeneration, PerturbationR, AnnealingParam) ->
  extract_cur_gen_idps(Ids, AgentGeneration, AgentGeneration, PerturbationR, AnnealingParam, []).

%%-----------------------------------------------------------------------------
%% @doc all_random first composes a list of tuples from NIds and their
%%      spreads, and then creates a sublist by choosing each element with a
%%      probability of 1/math:sqrt(Tot_neurons).
%% @end
%%-----------------------------------------------------------------------------
-spec all_random([{actuator | neuron, {float()}}], integer(), float(), float())
-> [{models:neuron_id(), float()}].
all_random(Ids, AGeneration, PerturbationR, AnnealingP) ->
  ChosenIdPs = extract_cur_gen_idps(Ids, AGeneration, AGeneration, PerturbationR, AnnealingP, []),
  MutationP = 1 / math:sqrt(length(ChosenIdPs)),
  choose_random_idps(MutationP, ChosenIdPs).

%%%============================================================================
%%% Internal functions
%%%============================================================================

chosen_idps(Ids, AgentGeneration, PerturbationRange, AnnealingP) ->
  AgeLimit = math:sqrt(1 / rand:uniform()),
  case extract_cur_gen_idps(Ids, AgentGeneration, AgeLimit, PerturbationRange, AnnealingP, []) of
    [] ->
      [Id | _] = Ids,
      [{Id, PerturbationRange * math:pi()}];
    ExtractedIdPs ->
      ExtractedIdPs
  end.

%%-----------------------------------------------------------------------------
%% @doc choose_random_idps accepts a mutation probability parameter and a
%%      list of tuples composed of neuron ids and their spreads, and then
%%      selects from this list randomly with a probability MutationP,
%%      composing a new sub list.
%% @end
%%-----------------------------------------------------------------------------
choose_random_idps(MutationP, IdPs) ->
  case choose_random_idps(IdPs, MutationP, []) of
    [] ->
      {Id, Spread} = lists:nth(rand:uniform(length(IdPs)), IdPs),
      [{Id, Spread}];
    Acc ->
      Acc
  end.

%%-----------------------------------------------------------------------------
%% @doc choose_random_idps accepts a mutation probability parameter and a
%%      list of tuples composed of neuron ids and their spreads, and then
%%      selects from this list randomly with a probability MutationP,
%%      composing a new sub list.
%% @end
%%-----------------------------------------------------------------------------
choose_random_idps([{Id, Spread} | IdPs], MutationP, Acc) ->
  UAcc = case rand:uniform() < MutationP of
    true ->
      [{Id, Spread} | Acc];
    false ->
      Acc
  end,
  choose_random_idps(IdPs, MutationP, UAcc);
choose_random_idps([], _MutationP, Acc) ->
  Acc.

%%-----------------------------------------------------------------------------
%% @doc The extract_cur_gen_idps composes an id pool from neurons and
%%      actuators who are younger than the AgeLimit parameter. This is
%%      calculated by comparing the generation when they were created or
%%      touched by mutation, with that of the agent which ages with every
%%      topological mutation phase. Id pool accumulates not just the neurons
%%      but also the spread which will be used for the synaptic weight
%%      perturbation. The spread is calculated by multiplying the
%%      perturbation_range variable by math:pi(), and then multiplied by the
%%      annealing factor which is math:pow(AnnealingParameter, Age).
%%      Annealing parameter is less than 1, thus the greater the age of the
%%      neuron, the lower the Spread will be.
%% @end
%%-----------------------------------------------------------------------------
extract_cur_gen_idps([Id | Ids], Generation, AgeLimit, PR, AP, Acc) ->
  Gen = case Id of
    {neuron, _} ->
      N = db:read(Id, neuron),
      models:get(generation, N);
    {actuator, _} ->
      A = db:read(Id, actuator),
      models:get(generation, A)
  end,
  case Gen >= (Generation - AgeLimit) of
    true ->
      Age = Generation - Gen,
      Spread = PR * math:pi() * math:pow(AP, Age),
      extract_cur_gen_idps(Ids, Generation, AgeLimit, PR, AP, [{Id, Spread} | Acc]);
    false ->
      extract_cur_gen_idps(Ids, Generation, AgeLimit, PR, AP, Acc)
  end;
extract_cur_gen_idps([], _Generation, _AgeLimit, _PR, _AP, Acc) ->
  Acc.