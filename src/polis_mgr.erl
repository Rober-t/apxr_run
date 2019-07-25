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
%%% @doc The polis_mgr process represents an interfacing point with the
%%%      neuroevolutionary platform infrastructure. The module contains the
%%%      functions that perform general, global tasks. Because there should be
%%%      there needs to be only a single polis_mgr per node, representing
%%%      a single neuroevolutionary platform per node. The following list
%%%      summarizes the types of functions we want to be able to execute
%%%      through the polis_mgr module:
%%%       (1) Start all the neuroevolutionary platform supporting processes
%%%       (2) Stop and shut down the neuroevolutionary platform
%%%      The PolisMgr is the infrastructure and the system within which the
%%%      the NN based agents, and the scapes they interface with,
%%%      exist. It is for this reason that it was given the name 'polis', an
%%%      independent and self governing city state of intelligent agents.
%%%      Perhaps at some future time when multiple such systems are running on
%%%      different nodes, each 'polis' will have its own id, and each polis_mgr
%%%      will concentrate on some particular goal towards which the
%%%      neuroevolutionary system is aimed. We give this 'polis' the name:
%%%      “MATHEMA”, which stands for knowledge and learning.
%%% @end
%%%----------------------------------------------------------------------------
-module(polis_mgr).

-behaviour(gen_server).

%% Start/Stop
-export([
  start_link/0
]).

%% API
-export([
  stop/0,
  prep/1,
  setup/1,
  backup_and_shutdown/0
]).

%% gen_server callbacks
-export([
  init/1,
  handle_call/3,
  handle_cast/2
]).

%% Xref
-ignore_xref([
  start_link/0,
  stop/0,
  prep/1,
  setup/1
]).

%%=============================================================================
%% Types
%%=============================================================================

-record(polis_mgr_state, {}).

-type state() :: #polis_mgr_state{}.

%%%============================================================================
%%% API
%%%============================================================================

%%-----------------------------------------------------------------------------
%% @doc The start_link first checks whether a polis_mgr process has already been
%%      spawned, by checking if one is registered. If it's not, then the
%%      gen_server:start_link function starts up the neuroevolutionary
%%      platform. Otherwise, it returns an error.
%% @end
%%-----------------------------------------------------------------------------
-spec start_link() -> {error, string()} | {ok, pid()}.
start_link() ->
  case whereis(polis_mgr) of
    undefined ->
      gen_server:start_link({local, ?MODULE}, ?MODULE, [], []);
    Pid ->
      logr:error({polis_mgr, start_link, error, "PolisMgr already online", [Pid]}),
      {error, "PolisMgr already running."}
  end.

%%-----------------------------------------------------------------------------
%% @doc The prep function first checks whether a polis_mgr process is online.
%%      If there is an online polis_mgr process running on the node, then the
%%      prep function function preps the system. Otherwise, it returns an error.
%% @end
%%-----------------------------------------------------------------------------
-spec prep(iodata()) -> {error, string()} | ok.
prep(Tarball) ->
  case whereis(polis_mgr) of
    undefined ->
      logr:error({polis_mgr, prep, error, "PolisMgr cannot prep, it is not online", []}),
      {error, "PolisMgr not online"};
    Pid ->
      gen_server:call(Pid, {prep, Tarball}, 15000)
  end.

%%-----------------------------------------------------------------------------
%% @doc The setup function first checks whether a polis_mgr process is online.
%%      If there is an online polis_mgr process running on the node, then the
%%      setup function function configures the system and starts the public
%%      scape if any. Otherwise, it returns an error.
%% @end
%%-----------------------------------------------------------------------------
-spec setup(binary()) -> {error, string()} | ok.
setup(Config) ->
  case whereis(polis_mgr) of
    undefined ->
      logr:error({polis_mgr, setup, error, "PolisMgr cannot setup, it is not online", []}),
      {error, "PolisMgr not online"};
    Pid ->
      gen_server:call(Pid, {setup, Config}, 15000)
  end.

%%-----------------------------------------------------------------------------
%% @doc Backs up the DB and shuts down the application.
%% @end
%%-----------------------------------------------------------------------------
-spec backup_and_shutdown() -> {error, string()} | ok.
backup_and_shutdown() ->
  logr:info({polis_mgr, status, ok, "backing up DB and shutting down", []}),
  gen_server:cast(?MODULE, backup_and_shutdown).

%%-----------------------------------------------------------------------------
%% @doc All applications are taken down smoothly, all code is unloaded, and all
%%      ports are closed before the system terminates by calling halt(Status).
%%      The stop function first checks whether a polis_mgr process is online.
%%      If there is an online polis_mgr process running on the node, then the
%%      stop function sends a signal to it requesting it to stop. Otherwise,
%%      it shutdowns immediately.
%% @end
%%-----------------------------------------------------------------------------
-spec stop() -> {error, string()} | ok.
stop() ->
  case whereis(polis_mgr) of
    undefined ->
      logr:error({polis_mgr, stop, error, "polis_mgr not online", []}),
      logr:info({polis_mgr, status, ok, "shutting down", []}),
      apxr_run_app:stop(#{});
    Pid ->
      logr:info({polis_mgr, status, ok, "shutting down", []}),
      gen_server:cast(Pid, {stop, external})
  end.

%%%============================================================================
%%% gen_server callbacks
%%%============================================================================

%%-----------------------------------------------------------------------------
%% @private
%% @doc Initializes the polis_mgr then drops into the main gen_server loop.
%% @end
%%-----------------------------------------------------------------------------
-spec init([]) -> {ok, state()}.
init([]) ->
  InitState = #polis_mgr_state{},
  logr:info({polis_mgr, init, ok, undefined, []}),
  {ok, InitState}.

%%-----------------------------------------------------------------------------
%% @private
%% @doc Handle call messages.
%% @end
%%-----------------------------------------------------------------------------
-spec handle_call({setup, binary()} | {prep, iodata(), state()} | term(),
  {pid(), term()}, state()) -> {reply, ok, state()}.
handle_call({prep, Tarball}, _F, State) ->
  do_prep(Tarball),
  {reply, ok, State};

handle_call({setup, Config}, _F, State) ->
  do_setup(Config),
  {reply, ok, State};

handle_call(Request, From, State) ->
  logr:warning({polis_mgr, msg, error, "unexpected handle_call", [Request, From]}),
  {reply, ok, State}.

%%-----------------------------------------------------------------------------
%% @private
%% @doc Handle cast messages.
%% @end
%%-----------------------------------------------------------------------------
-spec handle_cast({backup_and_shutdown, state()} | {stop, normal | shutdown}, state()) ->
{noreply, state()} | {stop, normal | shutdown, state()}.
handle_cast(backup_and_shutdown, _State) ->
  db:backup(),
  timer:sleep(45000),
  apxr_run_app:stop(#{});

handle_cast({stop, external}, _State) ->
  timer:sleep(5000),
  apxr_run_app:stop(#{});

handle_cast({stop, normal}, State) ->
  {stop, normal, State};

handle_cast({stop, shutdown}, State) ->
  {stop, shutdown, State}.

%%%============================================================================
%%% Internal functions
%%%============================================================================

do_prep(Tarball) when is_binary(Tarball); is_list(Tarball) ->
  {ok, #{checksum := _C, metadata := _M, contents := C}} = apxr_tarball:unpack(Tarball, memory),
  [code:load_binary(list_to_atom(filename:rootname(F)), list_to_atom(filename:rootname(F)), B)
  || {F, B} <- C],
  ok.

do_setup(Config) ->
  EConfig1 = jsx:decode(jsx:encode(Config), [return_maps, {labels, attempt_atom}]),
  EConfig2 = atomize(EConfig1),
  set_env_vars(EConfig2),
  logr:info({polis_mgr, status, ok, "set_env_vars", []}),
  maybe_start_public_scape().

set_env_vars(Config) ->
  ExpConfig = maps:get(exp_parameters, Config),
  PMPConfig = maps:get(pm_parameters, Config),
  InitConsConfig = maps:get(init_constraints, Config),
  % polis
  application:set_env(apxr_run, build_tool, maps:get(build_tool, ExpConfig)),
  application:set_env(apxr_run, identifier, maps:get(identifier, ExpConfig)),
  application:set_env(apxr_run, runs, maps:get(runs, ExpConfig)),
  application:set_env(apxr_run, public_scape, maps:get(public_scape, ExpConfig)),
  application:set_env(apxr_run, min_pimprovement, maps:get(min_pimprovement, ExpConfig)),
  application:set_env(apxr_run, search_params_mut_prob, maps:get(search_params_mut_prob, ExpConfig)),
  application:set_env(apxr_run, output_sat_limit, maps:get(output_sat_limit, ExpConfig)),
  application:set_env(apxr_run, ro_signal, maps:get(ro_signal, ExpConfig)),
  application:set_env(apxr_run, fitness_stagnation, maps:get(fitness_stagnation, ExpConfig)),
  application:set_env(apxr_run, population_mgr_efficiency,
    maps:get(population_mgr_efficiency, ExpConfig)),
  application:set_env(apxr_run, re_entry_probability, maps:get(re_entry_probability, ExpConfig)),
  application:set_env(apxr_run, shof_ratio, maps:get(shof_ratio, ExpConfig)),
  application:set_env(apxr_run, selection_algorithm_efficiency,
    maps:get(selection_algorithm_efficiency, ExpConfig)),
  % pmp
  application:set_env(apxr_run, pmp, #{data => maps:get(data, PMPConfig)}),
  % init_cons
  application:set_env(apxr_run, constraints, InitConsConfig).

atomize(#{mutation_operators := _MutOps} = Map) -> % init_constraints have special constraints
  maps:fold(fun(K, V1, Acc) ->
    V2 = case {K, V1} of
      {mutation_operators, MutOps} ->
        [list_to_tuple(atomize(Op)) || Op <- MutOps];
      {tot_topological_mutations_fs, MutFs} ->
        [list_to_tuple(atomize(F)) || F <- MutFs];
      {tuning_duration_f, [DurF, Param]} ->
        {atomize(DurF), atomize(Param)};
      _ ->
        atomize(V1)
    end,
    maps:put(atomize(K), V2, Acc)
  end, #{}, Map);
atomize(Map) when is_map(Map) ->
  maps:fold(fun(K, V, Acc) -> maps:put(atomize(K), atomize(V), Acc) end, #{}, Map);
atomize([_Head | _Tail] = Terms) ->
  [atomize(T) || T <- Terms];
atomize([]) ->
  [];
atomize(Term) when is_binary(Term) ->
  list_to_atom(binary_to_list(Term));
atomize(Term) when is_list(Term) ->
  list_to_atom(Term);
atomize(Term) ->
  Term.

maybe_start_public_scape() ->
  PublicScape = app_config:get_env(public_scape),
  case PublicScape of
    [] ->
      ok;
    [X, Y, Width, Height, ModName] ->
      scape_mgr_client:start_scape(X, Y, Width, Height, ModName)
  end.
