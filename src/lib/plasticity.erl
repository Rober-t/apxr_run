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
%%% @doc True learning is not achieved when a static NN is trained on some
%%%      data set through destruction and recreation by the exoself based on
%%%      its performance, but instead is the self organization of the NN, the
%%%      self adaptation and changing of the NN based on the information it is
%%%      processing. The learning rule, the way in which the neurons adapt
%%%      independently, the way in which their synaptic weights change based
%%%      on the neuron’s experience, that is true learning, and that is
%%%      neuroplasticity. There are numerous plasticity rules, some more
%%%      faithful to their biological counterparts than others, and some more
%%%      efficient than their biological counterparts. Plasticity rules or
%%%      functions that are defined in the plasticity module follow a
%%%      standardized plasticity function format, a function which accepts as
%%%      input the accumulated input vector IAcc, InputPidPs, and Output,
%%%      where IAcc is the input vector, InputPidPs is the associated vector
%%%      of synaptic weights, and the Output value is the neuron’s calculated
%%%      output. In response, the plasticity function produces an updated
%%%      set of synaptic weights, the updated InputPidPs vector. This
%%%      simulates the adaptation and the morphing of synaptic weights due to
%%%      the neuron’s interaction with the world, the neuron’s processing of
%%%      input signals. The neuroevolutionary system can generate NN based
%%%      agents with and without plasticity.
%%%      Note: The self_modulation_v1, self_modulation_v2, and
%%%      self_modulation_v3 are all very similar, mainly differing in the
%%%      parameter lists returned by the
%%%      PlasticityFunctionName(neural_parameters) function. All three of these
%%%      plasticity functions use the neuromodulation/5 function which
%%%      accepts the H, A, B, C, and D learning parameters, and updates the
%%%      synaptic weights of the neuron using the general Hebbian rule:
%%%      Updated_Wi = Wi + H*(A*Ii*Output + B*Ii + C*Output + D). The
%%%      self_modulation_v4 – v5 differ only in that the weight_parameters is a
%%%      list of length 2, and the A parameter is no longer specified in the
%%%      neural_parameters list, and is instead calculated by the second
%%%      dedicated modulatory neuron. The self_modulation_v6 function on the
%%%      other hand specifies the neural_parameters as an empty list, and the
%%%      weight_parameters list is of length 5, a single weight for every
%%%      embedded modulatory neuron.
%%% @end
%%%----------------------------------------------------------------------------
-module(plasticity).

%% API
-export([
  none/1, none/4,
  hebbian_w/1, hebbian_w/4,
  hebbian/1, hebbian/4,
  ojas_w/1, ojas_w/4,
  ojas/1, ojas/4,
  self_modulation_v1/1, self_modulation_v1/4,
  self_modulation_v2/1, self_modulation_v2/4,
  self_modulation_v3/1, self_modulation_v3/4,
  self_modulation_v4/1, self_modulation_v4/4,
  self_modulation_v5/1, self_modulation_v5/4,
  self_modulation_v6/1, self_modulation_v6/4,
  neuromodulation/1, neuromodulation/4
]).

%% Xref
-ignore_xref([
  none/1, none/4,
  hebbian_w/1, hebbian_w/4,
  hebbian/1, hebbian/4,
  ojas_w/1, ojas_w/4,
  ojas/1, ojas/4,
  self_modulation_v1/1, self_modulation_v1/4,
  self_modulation_v2/1, self_modulation_v2/4,
  self_modulation_v3/1, self_modulation_v3/4,
  self_modulation_v4/1, self_modulation_v4/4,
  self_modulation_v5/1, self_modulation_v5/4,
  self_modulation_v6/1, self_modulation_v6/4,
  neuromodulation/1, neuromodulation/4
]).

%%%============================================================================
%%% Configuration
%%%============================================================================

-define(SAT_LIMIT, math:pi() * 2).

%%%============================================================================
%%% API
%%%============================================================================

%%-----------------------------------------------------------------------------
%% @doc none returns a set of learning parameters needed by the none/1
%%      plasticity function. Since this function specifies that the neuron
%%      has no plasticity, the parameter lists are empty. When executed with
%%      the {NId, mutate} parameter, the function exits, since there is
%%      nothing to mutate. The exit allows for the neuroevolutionary system
%%      to try another mutation operator on the NN system.
%% @end
%%-----------------------------------------------------------------------------
-spec none(neural_parameters | weight_parameters) -> [].
none({_NId, mutate}) ->
  exit("Neuron does not support plasticity.");
none(neural_parameters) ->
  [];
none(weight_parameters) ->
  [].

%%-----------------------------------------------------------------------------
%% @doc The none returns the original InputPidPs to the caller.
%% @end
%%-----------------------------------------------------------------------------
-spec none(float(), [{pid(), [float()]}], [{pid(), [{float(), [float()]}]}], _)
-> [{pid(), [{float(), [float()]}]}].
none(_NeuralParameters, _IAcc, InputPidPs, _Output) ->
  InputPidPs.

%%-----------------------------------------------------------------------------
%% @doc hebbian_w function produces the necessary parameter list for the
%%      hebbian_w learning rule to operate. The parameter list for the
%%      simple hebbian_w learning rule is a parameter list composed of a
%%      single parameter H: [H], for every synaptic weight of the neuron.
%%      When hebbian_w is called with the parameter neural_parameters, it
%%      returns []. When hebbian_w/1 is executed with the {NId, mutate}
%%      parameter, the function goes through every parameter in the neuron's
%%      input_idps, and perturbs the parameter value using the specified
%%      spread (?SAT_LIMIT).
%% @end
%%-----------------------------------------------------------------------------
-spec hebbian_w(neural_parameters | weight_parameters | {models:neuron_id(), mutate}) ->
[float()] | [] | models:neuron().
hebbian_w({NId, mutate}) ->
  utils:random_seed(),
  N = db:read(NId, neuron),
  InputIdPs = models:get(input_idps, N),
  UInputIdPs = perturb_parameters(InputIdPs, ?SAT_LIMIT),
  models:set({input_idps, UInputIdPs}, N);
hebbian_w(neural_parameters) ->
  [];
hebbian_w(weight_parameters) ->
  [(rand:uniform() - 0.5)].

%%-----------------------------------------------------------------------------
%% @doc The hebbian_w function operates on each Input_PidP, calling the
%%      hebbian_w1 function which processes each of the complementary Is
%%      and WPs lists, producing the UpdatedWPs list in return, with the
%%      updated/adapted weights based on the hebbian_w learning rule.
%% @end
%%-----------------------------------------------------------------------------
-spec hebbian_w(float(), [{pid(), [float()]}], [{pid(), [{float(), [float()]}]}], _)
-> [{pid(), [{float(), [float()]}]}].
hebbian_w(_NeuralParameters, IAcc, InputPidPs, Output) ->
  hebbian_w1(IAcc, InputPidPs, Output, []).

%%-----------------------------------------------------------------------------
%% @doc hebbian function produces the necessary parameter list for the
%%      hebbian learning rule to operate. The parameter list for the
%%      standard hebbian learning rule is a parameter list composed of a
%%      single parameter H: [H], used by the neuron for all its synaptic
%%      weights. When hebbian/1 is called with the parameter
%%      weight_parameters, it returns []. When the function is executed with
%%      the {NId, mutate} parameter, it uses the perturb/4 function to
%%      perturb the parameter list, which in this case is a list composed of
%%      a single floating point parameter.
%% @end
%%-----------------------------------------------------------------------------
-spec hebbian(neural_parameters | weight_parameters | {models:neuron_id(), mutate}) ->
[float()] | models:neuron().
hebbian({NId, mutate}) ->
  utils:random_seed(),
  N = db:read(NId, neuron),
  {PFName, ParameterList} = models:get(pf, N),
  Spread = ?SAT_LIMIT * 10,
  MutationProb = 1 / math:sqrt(length(ParameterList)),
  UParameterList = perturb(ParameterList, MutationProb, Spread, []),
  UPF = {PFName, UParameterList},
  models:set({pf, UPF}, N);
hebbian(neural_parameters) ->
  [(rand:uniform() - 0.5)];
hebbian(weight_parameters) ->
  [].

%%-----------------------------------------------------------------------------
%% @doc The hebbian function operates on each Input_PidP, calling the
%%      hebbian function which processes each of the complementary Is and
%%      WPs lists, producing the UpdatedWPs list in return, with the
%%      updated/adapted weights based on the standard hebbian learning rule,
%%      using the neuron's single learning parameter H.
%% @end
%%-----------------------------------------------------------------------------
-spec hebbian([float()], [{pid(), [float()]}], [{pid(), [{float(), [float()]}]}], _)
-> [{pid(), [{float(), [float()]}]}].
hebbian([_M, H], IAcc, InputPidPs, Output) ->
  hebbian(H, IAcc, InputPidPs, Output, []).

%%-----------------------------------------------------------------------------
%% @doc The ojas_w function produces the necessary parameter list for the
%%      Oja's learning rule to operate. The parameter list for Oja's
%%      learning rule is a list composed of a single parameter H: [H] per
%%      synaptic weight. If the learning parameter is positive, then the
%%      post-synaptic neuron's synaptic weight increases if the two connected
%%      neurons produce output signals of the same sign. If the learning
%%      parameter is negative, and the two connected neurons produce output
%%      signals of the same sign, then the synaptic weight of the
%%      post-synaptic neuron, decreases in magnitude. Otherwise it increases.
%% @end
%%-----------------------------------------------------------------------------
-spec ojas_w(neural_parameters | weight_parameters | {models:neuron_id(), mutate}) ->
[float()] | models:neuron().
ojas_w({NId, mutate}) ->
  utils:random_seed(),
  N = db:read(NId, neuron),
  InputIdPs = models:get(input_idps, N),
  UInputIdPs = perturb_parameters(InputIdPs, ?SAT_LIMIT),
  models:set({input_idps, UInputIdPs}, N);
ojas_w(neural_parameters) ->
  [];
ojas_w(weight_parameters) ->
  [(rand:uniform() - 0.5)].

%%-----------------------------------------------------------------------------
%% @doc The ojas_w function operates on each InputPidP, calling the
%%      ojas_rule_w function which processes each of the complementary Is
%%      and WPs lists, producing the UpdatedWPs list in return, with the
%%      updated/adapted weights based on the Oja's learning rule, using each
%%      synaptic weight's distinct learning parameter.
%% @end
%%-----------------------------------------------------------------------------
-spec ojas_w(float(), [{pid(), [float()]}], [{pid(), [{float(), [float()]}]}], _)
-> [{pid(), [{float(), [float()]}]}].
ojas_w(_NeuralParameters, IAcc, InputPidPs, Output) ->
  ojas_w1(IAcc, InputPidPs, Output, []).

%%-----------------------------------------------------------------------------
%% @doc The oja function produces the necessary parameter list for the
%%      Oja's learning rule to operate. The parameter list for Oja's
%%      learning rule is a list composed of a single parameter H: [H], used
%%      by the neuron for all its synaptic weights. If the learning
%%      parameter is positive, and the two connected neurons produce output
%%      signals of the same sign, then the post-synaptic neuron's synaptic
%%      weight increases. Otherwise it decreases.
%% @end
%%-----------------------------------------------------------------------------
-spec ojas(neural_parameters | weight_parameters | {models:neuron_id(), mutate}) ->
[float()] | models:neuron().
ojas({NId, mutate}) ->
  utils:random_seed(),
  N = db:read(NId, neuron),
  {PFName, ParameterList} = models:get(pf, N),
  Spread = ?SAT_LIMIT * 10,
  MutationProb = 1 / math:sqrt(length(ParameterList)),
  UParameterList = perturb(ParameterList, MutationProb, Spread, []),
  UPF = {PFName, UParameterList},
  models:set({pf, UPF}, N);
ojas(neural_parameters) ->
  [(rand:uniform() - 0.5)];
ojas(weight_parameters) ->
  [].

%%-----------------------------------------------------------------------------
%% @doc The ojas function operates on each Input_PidP, calling the
%%      ojas_rule function which processes each of the complementary Is
%%      and WPs lists, producing the UpdatedWPs list in return, with the
%%      updated/adapted weights based on the standard Oja's learning rule.
%% @end
%%-----------------------------------------------------------------------------
-spec ojas([float()], [{pid(), [float()]}], [{pid(), [{float(), [float()]}]}], _)
-> [{pid(), [{float(), [float()]}]}].
ojas([_M, H], IAcc, InputPidPs, Output) ->
  ojas(H, IAcc, InputPidPs, Output, []).

%%-----------------------------------------------------------------------------
%% @doc  Given the general Hebbian learning rule for synaptic weight updating:
%%       Updated_Wi = Wi + H*(A*Ii*Output + B*Ii + C*Output + D), we can have
%%       multiple versions of this function. Version-1: where the secondary
%%       embedded neuron only outputs the H learning parameter, with the
%%       parameter A set to some predetermined constant value within the
%%       neural_parameters list, and B=C=D=0.
%% @end
%%-----------------------------------------------------------------------------
-spec self_modulation_v1(neural_parameters | weight_parameters | {models:neuron_id(), mutate}) ->
[float()] | models:neuron().
self_modulation_v1({NId, mutate}) ->
  utils:random_seed(),
  N = db:read(NId, neuron),
  InputIdPs = models:get(input_idps, N),
  UInputIdPs = perturb_parameters(InputIdPs, ?SAT_LIMIT),
  models:set({input_idps, UInputIdPs}, N);
self_modulation_v1(neural_parameters) ->
  A = 0.1,
  B = 0,
  C = 0,
  D = 0,
  [A, B, C, D];
self_modulation_v1(weight_parameters) ->
  [(rand:uniform() - 0.5)].

-spec self_modulation_v1([float()], [{pid(), [float()]}], [{pid(),
  [{float(), [float()]}]}], _) -> [{pid(), [{float(), [float()]}]}].
self_modulation_v1([_M, A, B, C, D], IAcc, InputPidPs, Output) ->
  H = math:tanh(dot_product_v1(IAcc, InputPidPs)),
  neuromodulation([H, A, B, C, D], IAcc, InputPidPs, Output, []).

%%-----------------------------------------------------------------------------
%% @doc  Version-2: where A is generated randomly when generating the
%%       neural_parameters list, and B=C=D=0.
%% @end
%%-----------------------------------------------------------------------------
-spec self_modulation_v2(neural_parameters | weight_parameters | {models:neuron_id(), mutate}) ->
[float()] | models:neuron().
self_modulation_v2({NId, mutate}) ->
  utils:random_seed(),
  N = db:read(NId, neuron),
  {PFName, [A | ParameterList]} = models:get(pf, N),
  [UA] = perturb([A], 0.5, ?SAT_LIMIT * 10, []),
  UPF = {PFName, [UA | ParameterList]},
  InputIdPs = models:get(input_idps, N),
  UInputIdPs = perturb_parameters(InputIdPs, ?SAT_LIMIT),
  models:set([{pf, UPF}, {input_idps, UInputIdPs}], N);
self_modulation_v2(neural_parameters) ->
  A = (rand:uniform() - 0.5),
  B = 0,
  C = 0,
  D = 0,
  [A, B, C, D];
self_modulation_v2(weight_parameters) ->
  [(rand:uniform() - 0.5)].

-spec self_modulation_v2([float()], [{pid(), [float()]}],
  [{pid(), [{float(), [float()]}]}], _) -> [{pid(), [{float(), [float()]}]}].
self_modulation_v2([_M, A, B, C, D], IAcc, InputPidPs, Output) ->
  H = math:tanh(dot_product_v1(IAcc, InputPidPs)),
  neuromodulation([H, A, B, C, D], IAcc, InputPidPs, Output, []).

%%-----------------------------------------------------------------------------
%% @doc  Version-3: where B, C, and D are also generated randomly in the
%%       neural_parameters list.
%% @end
%%-----------------------------------------------------------------------------
-spec self_modulation_v3(neural_parameters | weight_parameters | {models:neuron_id(), mutate}) ->
[float()] | models:neuron().
self_modulation_v3({NId, mutate}) ->
  utils:random_seed(),
  N = db:read(NId, neuron),
  {PFName, ParameterList} = models:get(pf, N),
  MSpread = ?SAT_LIMIT * 10,
  MutationProb = 1 / math:sqrt(length(ParameterList)),
  UParameterList = perturb(ParameterList, MutationProb, MSpread, []),
  UPF = {PFName, UParameterList},
  InputIdPs = models:get(input_idps, N),
  UInputIdPs = perturb_parameters(InputIdPs, ?SAT_LIMIT),
  models:set([{pf, UPF}, {input_idps, UInputIdPs}], N);
self_modulation_v3(neural_parameters) ->
  A = (rand:uniform() - 0.5),
  B = (rand:uniform() - 0.5),
  C = (rand:uniform() - 0.5),
  D = (rand:uniform() - 0.5),
  [A, B, C, D];
self_modulation_v3(weight_parameters) ->
  [(rand:uniform() - 0.5)].

-spec self_modulation_v3([float()], [{pid(), [float()]}],
  [{pid(), [{float(), [float()]}]}], _) -> [{pid(), [{float(), [float()]}]}].
self_modulation_v3([_M, A, B, C, D], IAcc, InputPidPs, Output) ->
  H = math:tanh(dot_product_v1(IAcc, InputPidPs)),
  neuromodulation([H, A, B, C, D], IAcc, InputPidPs, Output, []).

%%-----------------------------------------------------------------------------
%% @doc  Version-4: where the weight_parameters generates a list of length 2,
%%       thus allowing the neuron to have 2 embedded modulatory neurons, one
%%       outputting a parameter we use for H, and another outputting the value
%%       we can use as A, with B=C=D=0.
%% @end
%%-----------------------------------------------------------------------------
-spec self_modulation_v4(neural_parameters | weight_parameters | {models:neuron_id(), mutate}) ->
[float()] | models:neuron().
self_modulation_v4({NId, mutate}) ->
  utils:random_seed(),
  N = db:read(NId, neuron),
  InputIdPs = models:get(input_idps, N),
  UInputIdPs = perturb_parameters(InputIdPs, ?SAT_LIMIT),
  models:set({input_idps, UInputIdPs}, N);
self_modulation_v4(neural_parameters) ->
  B = 0,
  C = 0,
  D = 0,
  [B, C, D];
self_modulation_v4(weight_parameters) ->
  [(rand:uniform() - 0.5), (rand:uniform() - 0.5)].

-spec self_modulation_v4([float()], [{pid(), [float()]}],
  [{pid(), [{float(), [float()]}]}], _) -> [{pid(), [{float(), [float()]}]}].
self_modulation_v4([_M, B, C, D], IAcc, InputPidPs, Output) ->
  {AccH, AccA} = dot_product_v4(IAcc, InputPidPs),
  H = math:tanh(AccH),
  A = math:tanh(AccA),
  neuromodulation([H, A, B, C, D], IAcc, InputPidPs, Output, []).

%%-----------------------------------------------------------------------------
%% @doc Version-5: Where B, C, and D are generated randomly by the
%%      PlasticityFunctionName(neural_parameters) function.
%% @end
%%-----------------------------------------------------------------------------
-spec self_modulation_v5(neural_parameters | weight_parameters | {models:neuron_id(), mutate}) ->
[float()] | models:neuron().
self_modulation_v5({NId, mutate}) ->
  utils:random_seed(),
  N = db:read(NId, neuron),
  {PFName, ParameterList} = models:get(pf, N),
  MSpread = ?SAT_LIMIT * 10,
  MutationProb = 1 / math:sqrt(length(ParameterList)),
  UParameterList = perturb(ParameterList, MutationProb, MSpread, []),
  UPF = {PFName, UParameterList},
  InputIdPs = models:get(input_idps, N),
  UInputIdPs = perturb_parameters(InputIdPs, ?SAT_LIMIT),
  models:set([{pf, UPF}, {input_idps, UInputIdPs}], N);
self_modulation_v5(neural_parameters) ->
  B = (rand:uniform() - 0.5),
  C = (rand:uniform() - 0.5),
  D = (rand:uniform() - 0.5),
  [B, C, D];
self_modulation_v5(weight_parameters) ->
  [(rand:uniform() - 0.5), (rand:uniform() - 0.5)].

-spec self_modulation_v5([float()], [{pid(), [float()]}],
  [{pid(), [{float(), [float()]}]}], _) -> [{pid(), [{float(), [float()]}]}].
self_modulation_v5([_M, B, C, D], IAcc, InputPidPs, Output) ->
  {AccH, AccA} = dot_product_v4(IAcc, InputPidPs),
  H = math:tanh(AccH),
  A = math:tanh(AccA),
  neuromodulation([H, A, B, C, D], IAcc, InputPidPs, Output, []).

%%-----------------------------------------------------------------------------
%% @doc Version-6: Where the weight_parameters produces a list of length 5,
%%      allowing the neuron to have 5 embedded modulatory neurons, whose
%%      outputs are used for H, A, B, C, and D.
%% @end
%%-----------------------------------------------------------------------------
-spec self_modulation_v6(neural_parameters | weight_parameters | {models:neuron_id(), mutate}) ->
[float()] | models:neuron().
self_modulation_v6({NId, mutate}) ->
  utils:random_seed(),
  N = db:read(NId, neuron),
  InputIdPs = models:get(input_idps, N),
  UInputIdPs = perturb_parameters(InputIdPs, ?SAT_LIMIT),
  models:set({input_idps, UInputIdPs}, N);
self_modulation_v6(neural_parameters) ->
  [];
self_modulation_v6(weight_parameters) ->
  H = (rand:uniform() - 0.5),
  A = (rand:uniform() - 0.5),
  B = (rand:uniform() - 0.5),
  C = (rand:uniform() - 0.5),
  D = (rand:uniform() - 0.5),
  [H, A, B, C, D].

-spec self_modulation_v6([float()], [{pid(), [float()]}],
  [{pid(), [{float(), [float()]}]}], _) -> [{pid(), [{float(), [float()]}]}].
self_modulation_v6([_M], IAcc, InputPidPs, Output) ->
  {AccH, AccA, AccB, AccC, AccD} = dot_product_v6(IAcc, InputPidPs),
  H = math:tanh(AccH),
  A = math:tanh(AccA),
  B = math:tanh(AccB),
  C = math:tanh(AccC),
  D = math:tanh(AccD),
  neuromodulation([H, A, B, C, D], IAcc, InputPidPs, Output, []).

%%-----------------------------------------------------------------------------
%% @doc Hebbian learning, which is a homosynaptic plasticity (also known as
%%      homotropic modulation) method, where the synaptic strength changes based
%%      on its history of activation. It is a synaptic weight update rule which
%%      is a function of its post- and pre- synaptic activity. But research
%%      shows that there is another approach to synaptic plasticity which nature
%%      has discovered, a highly dynamic and effective one, plasticity through
%%      neuromodulation. Neuromodulation is a form of heterosynaptic plasticity.
%%      In heterosynaptic plasticity the synaptic weights are changed due to the
%%      synaptic activity of other neurons, due to the modulating signals other
%%      neurons can produce to affect the given neuron’s synaptic weights.
%%      Since the modulatory signals will be used to compute a nonlinear value
%%      used to modulate the standard general Hebbian rule, we will not need
%%      any weight_parameters and so our plasticity function will produce an
%%      empty weight_parameters list. But we will need the general
%%      neural_parameters for the hebbian function, thus the neuromodulation/1
%%      function executed with the neuronal_parameters atom will return a list
%%      with 5 randomly generated (and later tuned and evolved) parameters:
%%      [H,A,B,C,D]. The neuromodulation/4 function is very simple, since it is
%%      executed with a list of all the necessary parameters to call the
%%      neuromodulation/5 function that applies the general hebbian rule to all
%%      the synaptic weights.
%% @end
%%-----------------------------------------------------------------------------
-spec neuromodulation(neural_parameters | weight_parameters | {models:neuron_id(), mutate}) ->
[float()] | models:neuron().
neuromodulation({NId, mutate}) ->
  utils:random_seed(),
  N = db:read(NId, neuron),
  {PFName, ParameterList} = models:get(pf, N),
  MSpread = ?SAT_LIMIT * 10,
  MutationProb = 1 / math:sqrt(length(ParameterList)),
  UParameterList = perturb(ParameterList, MutationProb, MSpread, []),
  UPF = {PFName, UParameterList},
  models:set({pf, UPF}, N);
neuromodulation(neural_parameters) ->
  H = (rand:uniform() - 0.5),
  A = (rand:uniform() - 0.5),
  B = (rand:uniform() - 0.5),
  C = (rand:uniform() - 0.5),
  D = (rand:uniform() - 0.5),
  [H, A, B, C, D];
neuromodulation(weight_parameters) ->
  [].

%%-----------------------------------------------------------------------------
%% @doc The neuromodulation/4 function is very simple, since it is
%%      executed with a list of all the necessary parameters to call the
%%      neuromodulation/5 function that applies the general hebbian rule to all
%%      the synaptic weights.
%% @end
%%-----------------------------------------------------------------------------
-spec neuromodulation([float()], [{float(), [float()]}], [{float(), [float()]}], _)
-> [{float(), [float()]}].
neuromodulation([M, H, A, B, C, D], IAcc, InputPidPs, Output) ->
  Modulator = scale_dzone(M, 0.33, ?SAT_LIMIT),
  neuromodulation([Modulator * H, A, B, C, D], IAcc, InputPidPs, Output, []).

%%%============================================================================
%%% Internal functions
%%%============================================================================

perturb_parameters(InputIdPs, Spread) ->
  TotParameters = lists:sum([lists:sum([length(Ps) || {_W, Ps} <- WPs])
    || {_InputId, WPs} <- InputIdPs]),
  MutationProb = 1 / math:sqrt(TotParameters),
  [{InputId, [{W, perturb(Ps, MutationProb, Spread, [])}
  || {W, Ps} <- WPs]} || {InputId, WPs} <- InputIdPs].

perturb([Val | Vals], MutationProb, Spread, Acc) ->
  case rand:uniform() < MutationProb of
    true ->
      UVal = sat((rand:uniform() - 0.5) * 2 * Spread + Val, Spread, Spread),
      perturb(Vals, MutationProb, Spread, [UVal | Acc]);
    false ->
      perturb(Vals, MutationProb, Spread, [Val | Acc])
  end;
perturb([], _MutationProb, _Spread, Acc) ->
  lists:reverse(Acc).

hebbian_w1([{IPid, Is} | IAcc], [{IPid, WPs} | InputPidPs], Output, Acc) ->
  UpdatedWPs = hebbrule_w(Is, WPs, Output, []),
  hebbian_w1(IAcc, InputPidPs, Output, [{IPid, UpdatedWPs} | Acc]);
hebbian_w1([], [], _Output, Acc) ->
  lists:reverse(Acc);
hebbian_w1([], [{bias, WPs}], _Output, Acc) ->
  lists:reverse([{bias, WPs} | Acc]).

%%-----------------------------------------------------------------------------
%% @private
%% @doc The hebbrule_w applies the hebbian learning rule to each weight,
%%      using the input value I, the neuron's calculated output Output, and
%%      its own distinct learning parameter H associated with each
%%      synaptic weight.
%% @end
%%-----------------------------------------------------------------------------
hebbrule_w([I | Is], [{W, [H]} | WPs], [Output], Acc) ->
  UpdatedW = functions:saturation(W + (H * I * Output), ?SAT_LIMIT),
  hebbrule_w(Is, WPs, [Output], [{UpdatedW, [H]} | Acc]);
hebbrule_w([], [], _Output, Acc) ->
  lists:reverse(Acc).

hebbian(H, [{IPid, Is} | IAcc], [{IPid, WPs} | InputPidPs], Output, Acc) ->
  UpdatedWPs = hebbrule(H, Is, WPs, Output, []),
  hebbian(H, IAcc, InputPidPs, Output, [{IPid, UpdatedWPs} | Acc]);
hebbian(_H, [], [], _Output, Acc) ->
  lists:reverse(Acc);
hebbian(_H, [], [{bias, WPs}], _Output, Acc) ->
  lists:reverse([{bias, WPs} | Acc]).

%%-----------------------------------------------------------------------------
%% @doc The hebbrule applies the hebbian learning rule to each weight,
%%      using the input value I, the neuron's calculated output Output, and
%%      the neuron's learning parameter H.
%% @end
%%-----------------------------------------------------------------------------
hebbrule(H, [I | Is], [{W, []} | WPs], [Output], Acc) ->
  UpdatedW = functions:saturation(W + (H * I * Output), ?SAT_LIMIT),
  hebbrule(H, Is, WPs, [Output], [{UpdatedW, []} | Acc]);
hebbrule(_H, [], [], _Output, Acc) ->
  lists:reverse(Acc).

ojas_w1([{IPid, Is} | IAcc], [{IPid, WPs} | InputPidPs], Output, Acc) ->
  UpdatedWPs = ojas_rule_w(Is, WPs, Output, []),
  ojas_w1(IAcc, InputPidPs, Output, [{IPid, UpdatedWPs} | Acc]);
ojas_w1([], [], _Output, Acc) ->
  lists:reverse(Acc);
ojas_w1([], [{bias, WPs}], _Output, Acc) ->
  lists:reverse([{bias, WPs} | Acc]).

%%-----------------------------------------------------------------------------
%% @doc The ojas_rule_w applies the ojas learning rule to each weight,
%%      using the input value I, the neuron's calculated output Output, and
%%      each weight's learning parameter H.
%% @end
%%-----------------------------------------------------------------------------
ojas_rule_w([I|Is], [{W, [H]} | WPs], [Output], Acc) ->
  UpdatedW = functions:saturation(W + (H * Output) * (I - Output * W), ?SAT_LIMIT),
  ojas_rule_w(Is, WPs, [Output], [{UpdatedW, [H]} | Acc]);
ojas_rule_w([], [], _Output, Acc) ->
  lists:reverse(Acc).

ojas(H, [{IPid, Is} | IAcc], [{IPid, WPs} | InputPidPs], Output, Acc) ->
  UpdatedWPs = ojas_rule(H, Is, WPs, Output, []),
  ojas(H, IAcc, InputPidPs, Output, [{IPid, UpdatedWPs} | Acc]);
ojas(_H, [], [], _Output, Acc) ->
  lists:reverse(Acc);
ojas(_H, [], [{bias, WPs}], _Output, Acc) ->
  lists:reverse([{bias, WPs} | Acc]).

%%-----------------------------------------------------------------------------
%% @doc The ojas_rule updates every synaptic weight using Oja's
%%      learning rule.
%% @end
%%-----------------------------------------------------------------------------
ojas_rule(H, [I|Is], [{W, []} | WPs], [Output], Acc) ->
  UpdatedW = functions:saturation(W + (H * Output) * (I - Output * W), ?SAT_LIMIT),
  ojas_rule(H, Is, WPs, [Output], [{UpdatedW, []} | Acc]);
ojas_rule(_H, [], [], _Output, Acc) ->
  lists:reverse(Acc).

neuromodulation([H, A, B, C, D], [{IPid, Is} | IAcc], [{IPid, WPs} | InputPidPs], Output, Acc) ->
  UpdatedWPs = genheb_rule([H, A, B, C, D], Is, WPs, Output, []),
  neuromodulation([H, A, B, C, D], IAcc, InputPidPs, Output, [{IPid, UpdatedWPs} | Acc]);
neuromodulation(_NeuralParameters, [], [], _Output, Acc) ->
  lists:reverse(Acc);
neuromodulation([H, A, B, C, D], [], [{bias, WPs}], Output, Acc) ->
  UpdatedWPs = genheb_rule([H, A, B, C, D], [1], WPs, Output, []),
  lists:reverse([{bias, UpdatedWPs} | Acc]).

genheb_rule([H, A, B, C, D], [I|Is], [{W, Ps} | WPs], [Output], Acc) ->
  UpdatedW = functions:saturation(W + H * ((A*I*Output) + (B*I) + (C*Output) + D), ?SAT_LIMIT),
  genheb_rule([H, A, B, C, D], Is, WPs, [Output], [{UpdatedW, Ps} | Acc]);
genheb_rule(_NeuralLearningParameters, [], [], _Output, Acc) ->
  lists:reverse(Acc).

dot_product_v1(IAcc, IPidPs) ->
  dot_product_v1(IAcc, IPidPs, 0).

dot_product_v1([{IPid, Input} | IAcc], [{IPid, WeightsP} | IPidPs], Acc) ->
  Dot = dot_v1(Input, WeightsP, 0),
  dot_product_v1(IAcc, IPidPs, Dot + Acc);
dot_product_v1([], [{bias, [{_Bias, [HBias]}]}], Acc) ->
  Acc + HBias;
dot_product_v1([], [], Acc) ->
  Acc.

dot_v1([I | Input], [{_W, [HW]} | Weights], Acc) ->
  dot_v1(Input, Weights, I * HW + Acc);
dot_v1([], [], Acc) ->
  Acc.

dot_product_v4(IAcc, IPidPs) ->
  dot_product_v4(IAcc, IPidPs, 0, 0).

dot_product_v4([{IPid, Input}|IAcc], [{IPid, WeightsP} | IPidPs], AccH, AccA) ->
  {DotH, DotA} = dot_v4(Input, WeightsP, 0, 0),
  dot_product_v4(IAcc, IPidPs, DotH + AccH, DotA + AccA);
dot_product_v4([], [{bias, [{_Bias, [HBias, ABias]}]}], AccH, AccA) ->
  {AccH + HBias, AccA + ABias};
dot_product_v4([], [], AccH, AccA) ->
  {AccH, AccA}.

dot_v4([I|Input], [{_W, [HW, AW]} | Weights], AccH, AccA) ->
  dot_v4(Input, Weights, I * HW + AccH, I * AW + AccA);
dot_v4([], [], AccH, AccA) ->
  {AccH, AccA}.

dot_product_v6(IAcc, IPidPs) ->
  dot_product_v6(IAcc, IPidPs, 0, 0, 0, 0, 0).

dot_product_v6([{IPid, Input} | IAcc], [{IPid, WeightsP}|IPidPs], AccH, AccA, AccB, AccC, AccD) ->
  {DotH, DotA, DotB, DotC, DotD} = dot_v6(Input, WeightsP, 0, 0, 0, 0, 0),
  dot_product_v6(IAcc, IPidPs, DotH + AccH, DotA + AccA, DotB + AccB, DotC + AccC, DotD + AccD);
dot_product_v6([], [{bias, [{_Bias, [HBias, ABias, BBias, CBias, DBias]}]}],
  AccH, AccA, AccB, AccC, AccD) ->
  {AccH + HBias, AccA + ABias, AccB + BBias, AccC + CBias, AccD + DBias};
dot_product_v6([], [], AccH, AccA, AccB, AccC, AccD) ->
  {AccH, AccA, AccB, AccC, AccD}.

dot_v6([I|Input], [{_W, [HW, AW, BW, CW, DW]} | Weights], AccH, AccA, AccB, AccC, AccD) ->
  dot_v6(Input, Weights, I * HW + AccH, I * AW + AccA, I * BW + AccB, I * CW + AccC, I * DW + AccD);
dot_v6([], [], AccH, AccA, AccB, AccC, AccD) ->
  {AccH, AccA, AccB, AccC, AccD}.

scale_dzone(Val, Threshold, MaxMagnitude) when Val > Threshold ->
  (functions:scale(Val, MaxMagnitude, Threshold) + 1) * MaxMagnitude / 2;
scale_dzone(Val, Threshold, MaxMagnitude) when Val < -Threshold ->
  (functions:scale(Val, -Threshold, -MaxMagnitude) -1) * MaxMagnitude / 2;
scale_dzone(_Val, _Threshold, _MaxMagnitude) ->
  0.

sat(Val, Max, Min) ->
  case Val > Max of
    true ->
      Max;
    false ->
      case Val < Min of
        true ->
          Min;
        false ->
          Val
      end
  end.