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
%%% @doc Utility functions.
%%% @end
%%%----------------------------------------------------------------------------
-module(utils).

%% API
-export([
  random_seed/0,
  safe_serialize_erlang/1,
  safe_binary_to_term/1,
  system_metrics/0,
  get_module/1,
  vec1_dominates_vec2/3, vec1_dominates_vec2/4
]).

%% Xref
-ignore_xref([
  random_seed/0,
  safe_serialize_erlang/1,
  safe_binary_to_term/1,
  system_metrics/0,
  get_module/1,
  vec1_dominates_vec2/3, vec1_dominates_vec2/4
]).

%%%============================================================================
%%% API
%%%============================================================================

%%-----------------------------------------------------------------------------
%% @doc Seed PRNG for the current process.
%% @end
%%-----------------------------------------------------------------------------
-spec random_seed() ->
  {#{next := fun((_) -> {_, _}), type := atom(), bits => non_neg_integer(),
    jump => fun(({_, _}) -> {_, _}), max => non_neg_integer(),
    uniform => fun(({_, _}) -> {_, _}), uniform_n => fun((pos_integer(), {_, _}) -> {_, _}),
    weak_low_bits => non_neg_integer()}, _}.
random_seed() ->
  %<<I1:32/unsigned-integer, I2:32/unsigned-integer, I3:32/unsigned-integer>> =
  %crypto:strong_rand_bytes(12),
  %rand:seed(exsplus, {I1, I2, I3}). %Cryptographically strong - slow
  rand:seed(exs1024s). % Not cryptographically strong - fast

%%-----------------------------------------------------------------------------
%% @doc Safe serialize Erlang.
%% @end
%%-----------------------------------------------------------------------------
safe_serialize_erlang(Term) ->
  T = binarify(Term),
  term_to_binary(T).

%%-----------------------------------------------------------------------------
%% @doc Safe binary to term.
%% @end
%%-----------------------------------------------------------------------------
safe_binary_to_term(Binary) when is_binary(Binary) ->
  try
    Term = binary_to_term(Binary),
    safe_terms(Term),
    {ok, Term}
  catch
    _:_ ->
      throw(malformed_erlang)
  end.

%%-----------------------------------------------------------------------------
%% @doc Return system metrics.
%% @end
%%-----------------------------------------------------------------------------
-spec system_metrics() -> map().
system_metrics() ->
  #{
    memory => [{V, recon_alloc:memory(V)} || V <- [used, allocated, unused, usage]],
    scheduler_usage => recon:scheduler_usage(1000)
  }.

%%-----------------------------------------------------------------------------
%% @doc Return correct module syntax based on SDK environment configuration.
%% @end
%%-----------------------------------------------------------------------------
-spec get_module(atom()) -> atom().
get_module(Module) when is_atom(Module) ->
  case app_config:get_env(build_tool) of
    erlang ->
      Module;
    elixir ->
      Module1 = binary_to_list('Elixir.Macro':camelize(atom_to_binary(Module, utf8))),
      maybe_add_elixir_prefix(Module1)
  end.

%%-----------------------------------------------------------------------------
%% @doc vec1_dominates_vec2 function.
%% @end
%%-----------------------------------------------------------------------------
-spec vec1_dominates_vec2([float()], [float()], float()) -> boolean().
vec1_dominates_vec2(A, B, MIP) ->
  VecDif = vec1_dominates_vec2(A, B, MIP, []),
  TotElems = length(VecDif),
  DifElems = length([Val || Val <- VecDif, Val > 0]),
  case DifElems of
    TotElems -> % Complete Superiority
      true;
    0 -> % Complete Inferiority
      false;
    _ -> % Variation, Pareto front
      false
  end.

%%-----------------------------------------------------------------------------
%% @doc vec1_dominates_vec2 vector diff function.
%% @end
%%-----------------------------------------------------------------------------
-spec vec1_dominates_vec2([float()], [float()], float(), [float()]) -> [float()].
vec1_dominates_vec2([Val1 | Vec1], [Val2 | Vec2], MIP, Acc) ->
  vec1_dominates_vec2(Vec1, Vec2, MIP, [Val1 - (Val2 + Val2 * MIP) | Acc]);
vec1_dominates_vec2([], [], _MIP, Acc) ->
  Acc.

%%%============================================================================
%%% Internal functions
%%%============================================================================

maybe_add_elixir_prefix(Module) ->
  case string:str(Module, "Elixir.") of
    0 ->
      list_to_atom("Elixir." ++ Module);
    _ ->
      list_to_atom(Module)
  end.

safe_terms(List) when is_list(List) ->
  safe_list(List);
safe_terms(Tuple) when is_tuple(Tuple) ->
  safe_tuple(Tuple, tuple_size(Tuple));
safe_terms(Map) when is_map(Map) ->
  F = fun(Key, Value, Acc) ->
    safe_terms(Key),
    safe_terms(Value),
    Acc
  end,
  maps:fold(F, Map, Map);
safe_terms(Other)
    when is_atom(Other) or is_number(Other) or is_bitstring(Other)
    or is_pid(Other) or is_reference(Other) ->
  Other;
safe_terms(_Other) ->
  throw(safe_terms).
safe_list([]) ->
  ok;

safe_list([H | T]) when is_list(T) ->
  safe_terms(H),
  safe_list(T);
safe_list([H | T]) ->
  safe_terms(H),
  safe_terms(T).

safe_tuple(_tuple, 0) ->
  ok;
safe_tuple(Tuple, N) ->
  safe_terms(element(N, Tuple)),
  safe_tuple(Tuple, N - 1).

binarify(Binary) when is_binary(Binary) ->
  Binary;
binarify(Number) when is_number(Number) ->
  Number;
binarify(Atom) when (Atom == nil) or is_boolean(Atom) ->
  Atom;
binarify(Atom) when is_atom(Atom) ->
  atom_to_binary(Atom, utf8);
binarify(List) when is_list(List) ->
  [binarify(Elem) || Elem <- List];
binarify(Tuple) when is_tuple(Tuple) ->
  [binarify(Elem) || Elem <- tuple_to_list(Tuple)];
binarify(Map) when is_map(Map) ->
  maps:fold(fun(K, V, Acc) -> maps:put(binarify(K),  binarify(V), Acc) end, #{}, Map).