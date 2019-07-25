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
%%% @doc The signal_aggregator module contains the various aggregation
%%%      functions. An aggregation function is a function that in some manner
%%%      gathers the input signal vectors, does something with it and the
%%%      synaptic weights, and then produces a scalar value. For example,
%%%      consider the dot product. The dot_product aggregation function
%%%      composes the scalar value by aggregating the input vectors, and then
%%%      calculating the dot product of the input vectors and the synaptic
%%%      weights. Another way to calculate a scalar value from the input and
%%%      weight vectors is by multiplying the corresponding input signals by
%%%      their weights, but instead of adding the resulting multiplied values,
%%%      we multiply them. The are many other types of aggregation functions
%%%      that could be created. We can also add normalizer functions, which
%%%      could normalize the input signals. The normalizers could be
%%%      implemented as part of the aggregator functions, although it
%%%      could be argued that even normalizing functions deserve their own
%%%      module.
%%% @end
%%%----------------------------------------------------------------------------
-module(signal_aggregator).

%% API
-export([
  dot_product/2,
  diff_product/2,
  mult_product/2
]).

%% Xref
-ignore_xref([
  dot_product/2,
  diff_product/2,
  mult_product/2
]).

%%%============================================================================
%%% API
%%%============================================================================

%%-----------------------------------------------------------------------------
%% @private
%% @doc The dot_product aggregation function is used in almost all artificial
%%      neural network implementations. It can be considered stable/proven.
%% @end
%%-----------------------------------------------------------------------------
-spec dot_product([{pid(), [float()]}], [{pid(), [float()]}]) -> float().
dot_product(IAcc, IPidPs) ->
  dot_product(IAcc, IPidPs, 0).

%%-----------------------------------------------------------------------------
%% @private
%% @doc The diff_product can be thought of as a neuron that looks not at the
%%      actual signal amplitudes, but the temporal difference in signal
%%      amplitudes. If the input signals have stabilized, then the neuron’s
%%      input is calculated as a 0, if there is a sudden change in the signal,
%%      the neuron will see it.
%% @end
%%-----------------------------------------------------------------------------
-spec diff_product([{pid(), [float()]}], [{pid(), [float()]}]) -> float().
diff_product(IAcc, IPIdPs) ->
  case get(diff_product) of
    undefined ->
      put(diff_product, IAcc),
      dot_product(IAcc, IPIdPs, 0);
    PrevIAcc ->
      put(diff_product, IAcc),
      DiffIAcc = input_diff(IAcc, PrevIAcc, []),
      dot_product(DiffIAcc, IPIdPs, 0)
  end.

%%-----------------------------------------------------------------------------
%% @private
%% @doc The worth of the mult_product aggregation function is questionable, and
%%      should be further studied through benchmarking and testing. If there is
%%      any worth to this type of signal aggregator, evolution will find it!
%% @end
%%-----------------------------------------------------------------------------
-spec mult_product([{pid(), [float()]}], [{pid(), [float()]}]) -> float().
mult_product(IAcc, IPidPs) ->
  mult_product(IAcc, IPidPs, 1).

%%%============================================================================
%%% Internal functions
%%%============================================================================

dot_product([{IPid, Input} | IAcc], [{IPid, WeightsP} | IPidPs], Acc) ->
  Dot = dot(Input, WeightsP, 0),
  dot_product(IAcc, IPidPs, Dot + Acc);
dot_product([], [{bias, [{Bias, _LPs}]}], Acc) ->
  Acc + Bias;
dot_product([], [], Acc) ->
  Acc.

dot([I | Input], [{W, _LPs} | WeightsP], Acc) ->
  dot(Input, WeightsP, I * W + Acc);
dot([], [], Acc) ->
  Acc.

input_diff([{IPId, Input} | IAcc], [{IPId, PrevInput} | PrevIAcc], Acc) ->
  VectorDiff = diff(Input, PrevInput, []),
  input_diff(IAcc, PrevIAcc, [{IPId, VectorDiff} | Acc]);
input_diff([], [], Acc) ->
  lists:reverse(Acc).

diff([A | Input], [B | PrevInput], Acc) ->
  diff(Input, PrevInput, [A - B | Acc]);
diff([], [], Acc) ->
  lists:reverse(Acc).

mult_product([{IPid, Input} | IAcc], [{IPid, WeightsP} | IPidPs], Acc) ->
  Dot = mult(Input, WeightsP, 1),
  mult_product(IAcc, IPidPs, Dot * Acc);
mult_product([], [{bias, [{Bias, _LPs}]}], Acc) ->
  Acc * Bias;
mult_product([], [], Acc) ->
  Acc.

mult([I | Input], [{W, _LPs} | WeightsP], Acc) ->
  mult(Input, WeightsP, I * W * Acc);
mult([], [], Acc) ->
  Acc.