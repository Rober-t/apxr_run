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
%%% @doc The tuning_duration module contains all the tuning duration functions,
%%%      functions which calculate how long the tuning phase must run. The
%%%      tuning duration function sets the max_attempts value, with the
%%%      function format being as follows: - Input: Neuron_Ids,
%%%      AgentGeneration - Output: Max_Attempts. The tuning duration
%%%      function can output a constant, which is what we used thus far.
%%%      It can output a value that is proportional to the number of neurons
%%%      composing the NN, or it can produce a value based on the number of
%%%      all neurons in the population.
%%%      NOTE: When creating tuning duration functions that take into account
%%%      NN's size, we must ensure that this factor skews the fitness towards
%%%      producing smaller NN systems, not larger. We do not want to reward
%%%      neural bloating. For example, if we create a tuning duration function
%%%      which uses the following equation: MaxAttempts = 100 * TotNeurons, we
%%%      will be giving an incentive for the NNs to bloat. Since just be adding
%%%      one extra neuron, the NN has 100 extra tries to improve its fitness,
%%%      chances are that it will be a bit more fit than its better counterparts
%%%      which did not get as many attempts.
%%%      The nsize_proportional and wsize_proportional functions have their
%%%      << exponential power >> parameters set to 0.5, and thus take the
%%%      square root of the number of neurons and weights respectively. Thus,
%%%      the NN systems which have a larger number of weights or neurons to
%%%      optimize, will have a larger number of chances, but just barely.
%%%      Hopefully, this approach will not overwrite and undermine the fitness
%%%      function, still push towards more concise topologies, while at the same
%%%      time provide for a few more optimization attempts to the larger
%%%      NN based agents, which need them due to having that many more synaptic
%%%      weight permutations which can be explored.
%%% @end
%%%----------------------------------------------------------------------------
-module(tuning_duration).

%% API
-export([
  const/3,
  wsize_proportional/3,
  nsize_proportional/3
]).

%% Xref
-ignore_xref([
  const/3,
  wsize_proportional/3,
  nsize_proportional/3
]).

%%%============================================================================
%%% API
%%%============================================================================

%%-----------------------------------------------------------------------------
%% @doc const returns the preset const max_attempts value.
%% @end
%%-----------------------------------------------------------------------------
-spec const(integer(), [models:neuron_id()], integer()) -> integer().
const(Parameter, _NIds, _Generation) ->
  Parameter. % ConstMaxAttempts

%%-----------------------------------------------------------------------------
%% @doc wsize_proportional calculates the max_attempts value based on the
%%      individual agent's parameters, in this case the max_attempts is
%%      proportional to the agent's number of weights belonging to the
%%      neurons which were added or mutated within the last 3 generations.
%% @end
%%-----------------------------------------------------------------------------
-spec wsize_proportional(float(), [models:neuron_id()], integer()) -> integer().
wsize_proportional(Parameter, NIds, Generation) ->
  Power = Parameter,
  ActiveNIds = extract_rec_gen_nids(NIds, Generation, 3, []),
  TotActiveNeuronWeights = extract_nweight_count(ActiveNIds, 0),
  round(10 + functions:sat(math:pow(TotActiveNeuronWeights, Power), 100.0, 0.0)).

%%-----------------------------------------------------------------------------
%% @doc nsize_proportional calculates the max_attempts to be proportional
%%      to the number of neurons which were within the last 3 generations
%%      mutated or added to the NN.
%% @end
%%-----------------------------------------------------------------------------
-spec nsize_proportional(float(), [models:neuron_id()], integer()) -> integer().
nsize_proportional(Parameter, NIds, Generation) ->
  Power = Parameter,
  TotNeurons = length(extract_rec_gen_nids(NIds, Generation, 3, [])),
  round(20 + functions:sat(math:pow(TotNeurons, Power), 100.0, 0.0)).

%%%============================================================================
%%% Internal functions
%%%============================================================================

%%-----------------------------------------------------------------------------
%% @doc extract_rec_gen_nids extracts the NIds of all neurons whose age is
%%      lower or equal to the AgeLimit.
%% @end
%%-----------------------------------------------------------------------------
extract_rec_gen_nids([NId | NIds], Generation, AgeLimit, Acc) ->
  N = db:read(NId, neuron),
  NeuronGen = models:get(generation, N),
  case NeuronGen >= (Generation - AgeLimit) of
    true ->
      extract_rec_gen_nids(NIds, Generation, AgeLimit, [NId | Acc]);
    false ->
      extract_rec_gen_nids(NIds, Generation, AgeLimit, Acc)
  end;
extract_rec_gen_nids([], _Generation, _AgeLimit, Acc) ->
  Acc.

%%-----------------------------------------------------------------------------
%% @doc extract_nweight_count counts the number of weights in total
%%      belonging to the list of neuron ids that the function was called with.
%% @end
%%-----------------------------------------------------------------------------
extract_nweight_count([NId | RecGenNIds], Acc) ->
  N = db:read(NId, neuron),
  InputIdPs = models:get(input_idps, N),
  TotWeights = lists:sum([length(Weights) || {_IId, Weights} <- InputIdPs]),
  extract_nweight_count(RecGenNIds, TotWeights + Acc);
extract_nweight_count([], Acc) ->
  Acc.