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
%%% @doc Since there are many ways to calculate TotMutations, we create the
%%%      tot_topological_mutations module, which can store the different
%%%      functions which can calculate this value.
%%% @end
%%%----------------------------------------------------------------------------
-module(tot_topological_mutations).

%% API
-export([
  ncount_exponential/2,
  ncount_linear/2
]).

%% Xref
-ignore_xref([
  ncount_exponential/2,
  ncount_linear/2
]).

%%%============================================================================
%%% API
%%%============================================================================

%%-----------------------------------------------------------------------------
%% @doc ncount_exponential calculates TotMutations by putting the size of
%%    the NN to some power Power.
%% @end
%%-----------------------------------------------------------------------------
-spec ncount_exponential(float(), models:agent_id()) -> pos_integer().
ncount_exponential(Power, AgentId) ->
  A = db:read(AgentId, agent),
  Cx = db:read(models:get(cx_id, A), cortex),
  TotNeurons = length(models:get(neuron_ids, Cx)),
  TotMutations = rand:uniform(round(math:pow(TotNeurons, Power))),
  TotMutations.

%%-----------------------------------------------------------------------------
%% @doc ncount_linear calculates TotMutations by multiplying the size of
%%    the NN by the value Multiplier.
%% @end
%%-----------------------------------------------------------------------------
-spec ncount_linear(float(), models:agent_id()) -> float().
ncount_linear(Multiplier, AgentId) ->
  A = db:read(AgentId, agent),
  Cx = db:read(models:get(cx_id, A), cortex),
  TotNeurons = length(models:get(neuron_ids, Cx)),
  TotMutations = TotNeurons * Multiplier,
  TotMutations.

%%%============================================================================
%%% Internal functions
%%%============================================================================