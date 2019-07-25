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
%%% @doc The population_mgr, a process that spawns a population of NN
%%%      systems, monitors their performance, applies a selection algorithm to
%%%      the NNs in the population, and generates the mutant offspring from
%%%      the fit NNs, while removing the unfit. The population_mgr module
%%%      is the one responsible for mapping the genotypes to their phenotypes.
%%%      A population is a group of agents, in a neuroevolutionary system
%%%      those agents are NN based based systems. The genotypes of our NN's
%%%      are represented as lists of records. Currently, in our system, each
%%%      NN genome is composed of a single cortex, one or more sensors, one or
%%%      more actuators, and one or more neurons. Each element of the NN system
%%%      knows what other elements it is connected to through element ids.
%%%      During one of our simulations we might want to start the experiment
%%%      with many different species. Since the NN's depend on their
%%%      morphologies, we can create a population with two different species,
%%%      each with it own morphology. Then, when the NN's are created in those
%%%      species, they would naturally start off with different sets available
%%%      to them and belonging to the particular species they were seeded in.
%%%      The offspring are created through cloning and mutation. Not all fit
%%%      agents are equal, some are more equal than others, some have a higher
%%%      fitness level. Though all the fit agents will survive to the next
%%%      generation, the number of offspring each agent creates will depend on
%%%      that agent's fitness. The population_mgr will decide how many offspring
%%%      to allocate to each agent. The offspring will be created by first
%%%      cloning the fit agent, and then by mutating the clone to produce a
%%%      a variation, a mutant, of it. The clone, with its own unique id, is
%%%      assigned to the same specie that its parent belongs to. Once all the
%%%      offspring are created, where "all" means the same number as was deleted
%%%      during the selection process, the new generation of agents is then
%%%      released back into the scape, or applied again to the problem. Then,
%%%      the evolutionary cycle repeats.
%%% @end
%%%----------------------------------------------------------------------------
-module(population_mgr).

% Required for fun2ms
-include_lib("stdlib/include/ms_transform.hrl").

%% Start/Stop
-export([
  start/1
]).

%% API
-export([
  agent_terminated/1,
  set_goal_reached/0,
  set_evaluations/4,
  validation_complete/2,
  set_op_tag/1
]).

%% Callbacks
-export([
  init/1,
  loop/0, loop/1,
  handle/2, handle/3,
  terminate/2
]).

%% Xref
-ignore_xref([
  loop/0, loop/1,
  handle/2, handle/3,
  terminate/2
]).

-ifdef(TEST).
-export([
  gather_stats/3,
  calculate_specie_fitness/1,
  calculate_specie_avg_nodes/1,
  calculate_specie_diversity/1,
  do_evaluations/9,
  do_termination_generational/5,
  do_termination_steady_state/5,
  do_termination_generational_continue/4
]).
-endif.

%% Xref
-ignore_xref([
  start/1
]).

%%%============================================================================
%%% Types
%%%============================================================================

-record(p_state, {
  op_modes :: models:op_modes(),
  evo_alg :: steady_state | generational,
  population_id :: models:population_id(),
  step_size :: non_neg_integer(),
  selection_algorithm :: atom(),
  survival_percentage :: non_neg_integer(),
  specie_size_limit :: non_neg_integer(),
  init_specie_size :: non_neg_integer(),
  generation_limit :: inf |non_neg_integer(),
  evaluations_limit :: non_neg_integer(),
  fitness_goal :: inf | float()
}).

-type state() :: {} | #p_state{}.

%%%============================================================================
%%% API
%%%============================================================================

%%-----------------------------------------------------------------------------
%% @doc Spawns a PopulationMgr process and calls init to initialize.
%% @end
%%-----------------------------------------------------------------------------
-spec start(node()) -> pid().
start(Node) ->
  PMP = app_config:get_env(pmp),
  Constraints = app_config:get_env(constraints),
  Pid = spawn_link(Node, ?MODULE, init, [{PMP, Constraints}]),
  register(population_mgr, Pid),
  Pid.

%%-----------------------------------------------------------------------------
%% @doc The agent_terminated accepts the requests sent by the agents
%%      which terminate after finishing with their evaluations. The function
%%      specializes in the "competition" selection algorithm, which is a
%%      generational selection algorithm. As a generation selection
%%      algorithm, it waits until the entire population has finished being
%%      evaluated, and only then selects the fit from the unfit, and creates
%%      the updated population of the next generation. The OpTag can be set
%%      from the outside to shutdown the population_mgr by setting it to
%%      done. Once an ending condition is reached, either through a
%%      generation limit, an evaluations limit, or fitness goal, the
%%      population_mgr exits normally. If the ending condition is not
%%      reached, the population_mgr spawns the new generation of agents
%%      and awaits again for all the agents in the population to complete
%%      their evaluations. If the OpTag is set to pause, it does not
%%      generate a new population, and instead goes into a waiting mode, and
%%      awaits to be restarted or terminated.
%% @end
%%-----------------------------------------------------------------------------
-spec agent_terminated(binary() | models:agent_id()) -> ok.
agent_terminated(AgentId) ->
  population_mgr ! {handle, {agent_terminated, AgentId}},
  ok.

%%-----------------------------------------------------------------------------
%% @doc The set_goal_reached function sets the goal_reached flag of the
%%      population_mgr to true.
%% @end
%%-----------------------------------------------------------------------------
-spec set_goal_reached() -> ok.
set_goal_reached() ->
  population_mgr ! {handle, {set_goal_reached}},
  ok.

%%-----------------------------------------------------------------------------
%% @doc The set_evaluations function is called after the agent has completed
%%      its evaluation run. It calculates the total number of evaluations,
%%      gathers stats, etc.
%% @end
%%-----------------------------------------------------------------------------
-spec set_evaluations(models:specie_id(), integer(), integer(), integer()) -> ok.
set_evaluations(SpecieId, AEA, AgentCycleAcc, AgentTimeAcc) ->
  population_mgr ! {handle, {set_evaluations, SpecieId, AEA, AgentCycleAcc, AgentTimeAcc}},
  ok.

%%-----------------------------------------------------------------------------
%% @doc The validation_complete function is called after the validation test
%%      run has completed. It returns the fitness score of the validation test
%%      agent.
%% @end
%%-----------------------------------------------------------------------------
-spec validation_complete(models:agent_id(), float()) -> ok.
validation_complete(AgentId, Fitness) ->
  population_mgr ! {handle, {validation_complete, AgentId, Fitness}},
  ok.

%%-----------------------------------------------------------------------------
%% @doc The population_mgr process accepts a pause command, which
%%      if it receives, it then goes into pause mode after all the agents
%%      have completed with their evaluations. The process can only go into
%%      pause mode if it is currently in the continue mode (its op_tag is
%%      set to continue). The population_mgr process can accept a
%%      continue command if its current op_tag is set to pause. When it
%%      receives a continue command, it summons all the agents in the
%%      population, and continues with its neuroevolution synchronization
%%      duties.
%% @end
%%-----------------------------------------------------------------------------
-spec set_op_tag(pause | continue) -> ok.
set_op_tag(OpTag) ->
  population_mgr ! {handle, {set_op_tag, OpTag}},
  ok.

%%%============================================================================
%%% Callbacks
%%%============================================================================

%%-----------------------------------------------------------------------------
%% @private
%% @doc In init the population_mgr process sets all the needed parameters
%%      within its #p_state record. The function first extracts all the
%%      AgentIds. Each agent is then spawned and activated, converted from
%%      genotype to phenotype in the summon_agents function. The
%%      summon_agents function summons the agents. Once the state record's
%%      parameters have been set, the function drops into the main gen_statem
%%      loop.
%% @end
%%-----------------------------------------------------------------------------
-spec init({models:pmp(), [models:constraint()]}) -> no_return().
init({PMP, SpecieCons}) ->
  utils:random_seed(),
  process_flag(trap_exit, true),
  init_population(models:get(population_id, PMP), models:get(init_specie_size, PMP), SpecieCons),
  logr:debug({population_mgr, init, ok, undefined, [PMP]}),
  self() ! {handle, {init_phase2, PMP}},
  loop().

%%-----------------------------------------------------------------------------
%% @private
%% @doc Receive and handle messages.
%% @end
%%-----------------------------------------------------------------------------
-spec loop() -> no_return().
loop() ->
  receive
    {handle, {init_phase2, PMP}} ->
      InitState = handle(init_phase2, PMP),
      loop(InitState)
  end.

%%-----------------------------------------------------------------------------
%% @private
%% @doc Receive and handle messages.
%% @end
%%-----------------------------------------------------------------------------
-spec loop(state()) -> no_return().
loop(State) ->
  receive
    {handle, {agent_terminated, AgentId}} ->
      EvoAlg = State#p_state.evo_alg,
      handle({agent_terminated, AgentId}, EvoAlg, State);

    {handle, {set_goal_reached}} ->
      handle(set_goal_reached, State),
      loop(State);

    {handle, {set_evaluations, SpecieId, AEA, AgtCycleAcc, AgtTimeAcc}} ->
      EvoAlg = State#p_state.evo_alg,
      handle({set_evaluations, SpecieId, AEA, AgtCycleAcc, AgtTimeAcc}, EvoAlg, State);

    {handle, {set_op_tag, OpTag}} ->
      handle({set_op_tag, OpTag}, State),
      loop(State);

    {'EXIT', _Pid, normal} ->
      ignore;

    {'EXIT', Pid, Reason} ->
      logr:debug({population_mgr, msg, ok, "exit received", [Pid, Reason]}),
      terminate(Reason, State);

    stop ->
      ets:foldl(fun({K, V, _}, ok) -> stop_agent({V, K}) end, ok, active_agents),
      terminate(normal, State)
  end.

%%-----------------------------------------------------------------------------
%% @private
%% @doc When the population_mgr process terminates, it states so,
%%      notifies with what op_tag it terminated with, all the states
%%      gathered, and then shuts down.
%% @end
%%-----------------------------------------------------------------------------
-spec terminate(atom(), state()) -> ok.
terminate(Reason, State) ->
  PopulationId = State#p_state.population_id,
  PS = ets:lookup_element(population_status, PopulationId, 2),
  TotEvaluations = models:get(tot_evaluations, PS),
  gather_stats(PopulationId, 0, State),
  P = db:read(PopulationId, population),
  T = models:get(trace, P),
  UT = models:set({tot_evaluations, TotEvaluations}, T),
  UP = models:set({trace, UT}, P),
  db:write(UP, population),
  logr:info({population_mgr, status, ok, "population_mgr terminated", [Reason]}),
  experiment_mgr_client:run_complete(PopulationId, UT),
  exit(Reason).

%%%============================================================================
%%% Internal functions
%%%============================================================================

handle(init_phase2, PMP) ->
  do_start_agents(PMP);

handle(set_goal_reached, State) ->
  do_set_goal_reached(State);

handle({set_op_tag, pause}, State) ->
  PopulationId = State#p_state.population_id,
  PS = ets:lookup_element(population_status, PopulationId, 2),
  case models:get(op_tag, PS) of
    pause ->
      ok;
    continue ->
      UPS = models:set({op_tag, pause}, PS),
      ets:insert(population_status, {PopulationId, UPS})
  end;

handle({set_op_tag, continue}, State) ->
  PS = ets:lookup_element(population_status, State#p_state.population_id, 2),
  case models:get(op_tag, PS) of
    continue ->
      ok;
    pause ->
      do_continue(State)
  end.

handle({agent_terminated, AgentId}, generational, State) ->
  do_agent_terminated_generational(AgentId, State);

handle({agent_terminated, AgentId}, steady_state, State) ->
  do_agent_terminated_steady_state(AgentId, State);

handle({set_evaluations, SpecieId, AEA, AgtCycleAcc, AgtTimeAcc}, EvoAlg, State) ->
  do_set_evaluations(SpecieId, AEA, AgtCycleAcc, AgtTimeAcc, EvoAlg, State).

do_start_agents(PMP) ->
  P = db:read(models:get(population_id, PMP), population),
  PopulationId = models:get(population_id, PMP),
  summon_agents(),
  T = models:get(trace, P),
  PS = models:population_status(#{
    op_tag => continue,
    pop_gen => 0,
    eval_acc => 0,
    cycle_acc => 0,
    time_acc => 0,
    tot_evaluations => 0,
    goal_reached => false
  }),
  ets:insert(population_status, {PopulationId, PS}),
  [ets:insert(evaluations, {SpecieId, 0}) || SpecieId <- models:get(specie_ids, P)],
  State = #p_state{
    op_modes = models:get(op_modes, PMP),
    evo_alg = models:get(evo_alg_f, P),
    population_id = PopulationId,
    step_size = models:get(step_size, T),
    selection_algorithm = models:get(selection_f, P),
    survival_percentage = models:get(survival_percentage, PMP),
    specie_size_limit = models:get(specie_size_limit, PMP),
    init_specie_size = models:get(init_specie_size, PMP),
    generation_limit = models:get(generation_limit, PMP),
    evaluations_limit = models:get(evaluations_limit, PMP),
    fitness_goal = models:get(fitness_goal, PMP)
  },
  State.

do_agent_terminated_generational(AgentId, S) ->
  PopulationId = S#p_state.population_id,
  OpModes = S#p_state.op_modes,
  Params = {S#p_state.specie_size_limit, S#p_state.selection_algorithm, S#p_state.generation_limit, S#p_state.evaluations_limit, S#p_state.fitness_goal},
  do_termination_generational(PopulationId, AgentId, Params, OpModes, S).

do_agent_terminated_steady_state(AgentId, S) ->
  PopulationId = S#p_state.population_id,
  OpModes = S#p_state.op_modes,
  Params = {S#p_state.evaluations_limit},
  do_termination_steady_state(PopulationId, AgentId, Params, OpModes, S).

do_set_goal_reached(S) ->
  PopulationId = S#p_state.population_id,
  PS = ets:lookup_element(population_status, PopulationId, 2),
  UPS = models:set({goal_reached, true}, PS),
  ets:insert(population_status, {PopulationId, UPS}).

do_set_evaluations(SpecieId, AEA, AgtCycleAcc, AgtTimeAcc, EvoAlg, S) ->
  PopId = S#p_state.population_id,
  OpModes = S#p_state.op_modes,
  StepSize = S#p_state.step_size,
  do_evaluations(PopId, StepSize, SpecieId, AEA, AgtCycleAcc, AgtTimeAcc, OpModes, EvoAlg, S).

do_termination_generational(PopulationId, {agent, UId}, {SpecieSizeLim, SelectionAlgorithm, GenLimit, EvalLimit, FitnessGoal}, OpModes, State) ->
  [{UId, agent, SpecieId}] = ets:lookup(active_agents, UId),
  ActiveCount = (length(ets:tab2list(active_agents)) - 1),
  true = ets:delete(active_agents, UId),
  true = ets:insert(inactive_agents, {UId, agent, SpecieId}),
  logr:info({population_mgr, status, ok, "agents_left", [ActiveCount]}),
  case ActiveCount == 0 of
    true ->
      intrapopulation_selection(PopulationId, SpecieSizeLim, SelectionAlgorithm),
      do_termination_generational_continue(PopulationId, {SpecieSizeLim, SelectionAlgorithm, GenLimit, EvalLimit, FitnessGoal}, OpModes, State);
    false ->
      loop(State)
  end.

do_termination_generational_continue(PopId, {_SpecieSizeLim, _SelectionAlgorithm, GenLimit, EvalLimit, FitnessGoal}, _OpModes, State) ->
  PS = ets:lookup_element(population_status, PopId, 2),
  OpTag = models:get(op_tag, PS),
  UPopGen = models:get(pop_gen, PS) + 1,
  logr:info({population_mgr, status, ok, "population generation ended", [UPopGen]}),
  case OpTag of
    continue ->
      SpecieIds = models:get(specie_ids, db:read(PopId, population)),
      SFitList = [models:get(fitness, db:read(SpecieId, specie)) || SpecieId <- SpecieIds],
      BestF = lists:nth(1, lists:reverse(lists:sort([MaxF || {_, _, MaxF, _} <- SFitList]))),
      logr:info({population_mgr, status, ok, "best fitness", [BestF]}),
      do_ending_condition_reached(UPopGen, GenLimit, EvalLimit, BestF, FitnessGoal, PopId, State);
    done ->
      logr:info({population_mgr, status, ok, "shutting down population_mgr", []}),
      PS = ets:lookup_element(population_status, PopId, 2),
      UPS = models:set([{pop_gen, UPopGen}], PS),
      ets:insert(population_status, {PopId, UPS}),
      terminate(normal, State);
    pause ->
      logr:info({population_mgr, status, ok, "population_mgr paused", []}),
      PS = ets:lookup_element(population_status, PopId, 2),
      UPS = models:set([{pop_gen, UPopGen}], PS),
      ets:insert(population_status, {PopId, UPS}),
      loop(State)
  end.

do_ending_condition_reached(UPopGen, GenLimit, EvalLimit, BestFitness, FitnessGoal, PopId, State) ->
  PS = ets:lookup_element(population_status, PopId, 2),
  TotEvaluations = models:get(tot_evaluations, PS),
  GoalReached = models:get(goal_reached, PS),
  case (UPopGen >= GenLimit) or (TotEvaluations >= EvalLimit) or
    fitness_goal_reached(BestFitness, FitnessGoal) or GoalReached of
      true -> % ENDING_CONDITION_REACHED
        logr:info({population_mgr, status, ok, "ending_condition_reached", []}),
        update_population_status(PopId, UPopGen),
        terminate(normal, State);
      false -> % IN_PROGRESS
        ets:foldl(fun({K, V, _}, ok) -> start_agent({V, K}, gt) end, ok, active_agents),
        update_population_status(PopId, UPopGen),
        loop(State)
  end.

do_termination_steady_state(PopulationId, AgentId, {EvalLimit}, OpModes, State) ->
  logr:debug({population_mgr, termination_steady_state, ok, "agent_terminated", [AgentId]}),
  A = db:read(AgentId, agent),
  SpecieId = models:get(specie_id, A),
  {agent, UId} = AgentId,
  ets:delete(active_agents, UId),
  S = db:read(SpecieId, specie),
  Distinguishers = models:get(hof_distinguishers, S),
  SHOF = models:get(hall_of_fame, S),
  {USHOF, _Losers} = update_shof(SHOF, [AgentId], Distinguishers, []),
  USpecie = models:set({hall_of_fame, USHOF}, S),
  db:write(USpecie, specie),
  do_termination_ss_tot_evals(USpecie, PopulationId, {EvalLimit}, OpModes, State).

do_termination_ss_tot_evals(Specie, PopulationId, {EvalLimit}, _OpModes, State) ->
  EFF = app_config:get_env(population_mgr_efficiency),
  PS = ets:lookup_element(population_status, PopulationId, 2),
  TotEvals = models:get(tot_evaluations, PS),
  GoalReached = models:get(goal_reached, PS),
  case (TotEvals >= EvalLimit) or GoalReached of
    true -> % DONE
      gather_stats(PopulationId, TotEvals, State),
      ets:foldl(fun({K, V, _}, ok) -> stop_agent({V, K}) end, ok, active_agents),
      terminate(normal, State);
    false -> % CONTINUE
      USHOF = models:get(hall_of_fame, Specie),
      SId = models:get(id, Specie),
      FScaled = [{models:get(main_fitness, Champ) / math:pow(models:get(tot_n, Champ), EFF),
      models:get(id, Champ)} || Champ <- USHOF],
      TotFitness = lists:sum([MainFitness || {MainFitness, _Id} <- FScaled]),
      [OffspringId] = selection_algorithm:choose_winners(SId, FScaled, TotFitness, [], [], 1),
      {agent, UId} = OffspringId,
      ets:insert(active_agents, {UId, agent, SId}),
      start_agent(OffspringId, gt),
      loop(State)
  end.

do_evaluations(PopulationId, StepSize, SpecieId, AEA, AgentCycleAcc, AgentTimeAcc, _OpModes, _EvoAlg, State) ->
  PS = ets:lookup_element(population_status, PopulationId, 2),
  TotEvals = models:get(tot_evaluations, PS),
  GoalReached = models:get(goal_reached, PS),
  EvalAcc = models:get(eval_acc, PS),
  CycleAcc = models:get(cycle_acc, PS),
  TimeAcc = models:get(time_acc, PS),
  AgentEvalAcc = case GoalReached of
    true ->
      0;
    false ->
      AEA
  end,
  UEvalAcc = EvalAcc + AgentEvalAcc,
  UCycleAcc = CycleAcc + AgentCycleAcc,
  UTimeAcc = TimeAcc + AgentTimeAcc,
  UTotEvaluations = TotEvals + AgentEvalAcc,
  SEvalAcc = ets:lookup_element(evaluations, SpecieId, 2),
  ets:insert(evaluations, {SpecieId, SEvalAcc + AgentEvalAcc}),
  case UEvalAcc >= StepSize of
    true ->
      gather_stats(PopulationId, UEvalAcc, State),
      logr:info({evaluations, status, ok, "total_evaluations", [UTotEvaluations]}),
      UPS = models:set([{eval_acc, 0}, {cycle_acc, 0}, {time_acc, 0}, {tot_evaluations, UTotEvaluations}], PS),
      ets:insert(population_status, {PopulationId, UPS}),
      loop(State);
    false ->
      PS = ets:lookup_element(population_status, PopulationId, 2),
      UPS = models:set([{eval_acc, UEvalAcc}, {cycle_acc, UCycleAcc}, {time_acc, UTimeAcc}, {tot_evaluations, UTotEvaluations}], PS),
      ets:insert(population_status, {PopulationId, UPS}),
      loop(State)
  end.

do_continue(State) ->
  utils:random_seed(),
  PopulationId = State#p_state.population_id,
  summon_agents(),
  PS = ets:lookup_element(population_status, PopulationId, 2),
  UPS = models:set([{op_tag, continue}], PS),
  ets:insert(population_status, {PopulationId, UPS}).

%%-----------------------------------------------------------------------------
%% @private
%% @doc The function init_population creates a new population with the id
%%      PopulationId, composed of length(SpecieConstraints) species, where
%%      each specie uses the particular specie constraint specified within
%%      the SpecieConstraints list. The function first checks if a
%%      population with the noted PopulationId already exists, if a
%%      population does exist, then the function first deletes it, and then
%%      creates a fresh one.
%% @end
%%-----------------------------------------------------------------------------
init_population(PopulationId, InitSpecieSize, SpecieConstraints) ->
  utils:random_seed(),
  case db:read(PopulationId, population) of
    not_found ->
      create_population(PopulationId, InitSpecieSize, SpecieConstraints);
    _ ->
      delete_population(PopulationId),
      ets:delete_all_objects(active_agents),
      ets:delete_all_objects(inactive_agents),
      logr:debug({population_mgr, init_population, ok, "population already exists. deleting", [PopulationId]}),
      create_population(PopulationId, InitSpecieSize, SpecieConstraints)
  end, ok.

create_population(PId, SpecieSize, SpecieCons) ->
  SpecieIds = [create_specie(PId, SpecCon, origin, SpecieSize) || SpecCon <- SpecieCons],
  [C | _] = SpecieCons,
  Population = models:population(#{
    id => PId,
    specie_ids => SpecieIds,
    morphologies => undefined,
    innovation_factor => undefined,
    evo_alg_f => models:get(population_evo_alg_f, C),
    selection_f => models:get(population_selection_f, C),
    trace => models:trace(#{
      stats => [],
      tot_evaluations => 0,
      step_size => 500
    })
  }),
  db:write(Population, population).

delete_population(PopulationId) ->
  P = db:read(PopulationId, population),
  SpecieIds = models:get(specie_ids, P),
  [delete_specie(SpecieId) || SpecieId <- SpecieIds],
  db:delete(PopulationId, population).

delete_specie(SpecieId) ->
  delete_agents(),
  db:delete(SpecieId, specie).

delete_agents() ->
  ets:foldl(fun({K, V, _}, ok) -> genotype:delete_agent({V, K}) end, ok, active_agents),
  ok.

%%-----------------------------------------------------------------------------
%% @private
%% @doc The create_specie generates length(SpecieConstraints) number
%%      of specie, each composed of SpecieSize number of agents. The
%%      create_specie/4 function is a simplified version which uses
%%      the default parameters to call the create_specie function.
%%      The create_specie/6 function constructs the agents using the
%%      genotype:construct_agent function, Once all the agents have been
%%      created, the function creates the specie record, fills in the required
%%      elements, writes the specie to database, and then finally returns the
%%      SpecieId to the caller.
%% @end
%%-----------------------------------------------------------------------------
create_specie(PopulationId, SpecieCon, Fingerprint, SpecieSize) ->
  SpecieId = {specie, genotype:unique_id()},
  create_specie(PopulationId, SpecieId, SpecieSize, [], SpecieCon, Fingerprint).

%%-----------------------------------------------------------------------------
%% @private
%% @doc The create_specie function constructs the agents using the
%%      genotype:construct_agent function. Once all the agents have been
%%      created, the function creates the specie record, fills in the required
%%      elements, writes the specie to database, and then finally returns the
%%      SpecieId to the caller.
%% @end
%%-----------------------------------------------------------------------------
create_specie(PopulationId, SpecieId, 0, IdAcc, SpecieCon, Fingerprint) ->
  logr:debug({population_mgr, create_specie, ok, "SpecieId", [SpecieId]}),
  logr:debug({population_mgr, create_specie, ok, "Morphology", [models:get(morphology, SpecieCon)]}),
  Specie = models:specie(#{
    id => SpecieId,
    population_id => PopulationId,
    fingerprint => Fingerprint,
    constraint => SpecieCon,
    fitness => undefined,
    innovation_factor => {0, 0},
    stats => [],
    seed_agent_ids => IdAcc,
    hof_distinguishers => [tot_n],
    specie_distinguishers => [tot_n],
    hall_of_fame => []
  }),
  db:write(Specie, specie),
  SpecieId;

create_specie(PopulationId, SpecieId, AgentIndex, IdAcc, SpecieCon, Fingerprint) ->
  AgentId = {agent, UId} = {agent, genotype:unique_id()},
  genotype:construct_agent(SpecieId, AgentId, SpecieCon),
  ets:insert(active_agents, {UId, agent, SpecieId}),
  create_specie(PopulationId, SpecieId, AgentIndex - 1, [AgentId | IdAcc], SpecieCon, Fingerprint).

%%-----------------------------------------------------------------------------
%% @private
%% @doc The calculate_specie_fitness function calculates the general fitness
%%      statistic of the specie, the average, max, min, and standard
%%      deviation of the specie's fitness. The function first composes a
%%      fitness list by accessing the fitness scores of each agent belonging
%%      to it, and then calculates the noted above statistics from that list,
%%      returning the tuple to the caller.
%% @end
%%-----------------------------------------------------------------------------
calculate_specie_fitness(SpecieId) ->
  ActiveAgents = select_agents_by_specie(SpecieId),
  FitnessAcc = calculate_fitness(ActiveAgents),
  {MaxFitness, MinFitness, AvgFitness, StdFitness} = case FitnessAcc of
    [] ->
      {[], [], [], inf};
    [AvgerageFitness] ->
      {[AvgerageFitness], [AvgerageFitness], [AvgerageFitness], inf};
    _ ->
      vector_basic_stats(FitnessAcc)
  end,
  {AvgFitness, StdFitness, MaxFitness, MinFitness}.

gather_stats(PopulationId, EvaluationsAcc, State) ->
  P = db:read(PopulationId, population),
  T = models:get(trace, P),
  TimeStamp = erlang:monotonic_time(),
  SpecieStats = [update_specie_stat(SpecieId, TimeStamp, State) || SpecieId <- models:get(specie_ids, P)],
  PopulationStats = models:get(stats, T),
  UPopulationStats = [SpecieStats | PopulationStats],
  UTotEvaluations = models:get(tot_evaluations, T) + EvaluationsAcc,
  UTrace = models:set([{stats, UPopulationStats}, {tot_evaluations, UTotEvaluations}], T),
  db:write(models:set({trace, UTrace}, P), population).

update_specie_stat(SpecieId, TimeStamp, State) ->
  SpecieEvaluations = ets:lookup_element(evaluations, SpecieId, 2),
  ets:insert(evaluations, {SpecieId, 0}),
  S = db:read(SpecieId, specie),
  {AvgNeurons, NeuronsStd} = calculate_specie_avg_nodes(SpecieId),
  {AvgFitness, FitnessStd, MaxFitness, MinFitness} = calculate_specie_fitness(SpecieId),
  SpecieDiversity = calculate_specie_diversity(SpecieId),
  {ValFitness, ChampionId} = validation_testing(SpecieId, State),
  Stat = models:stat(#{
    morphology => models:get(morphology, models:get(constraint, S)),
    specie_id => SpecieId,
    avg_neurons => AvgNeurons,
    std_neurons => NeuronsStd,
    avg_fitness => AvgFitness,
    std_fitness => FitnessStd,
    max_fitness => MaxFitness,
    min_fitness => MinFitness,
    avg_diversity => SpecieDiversity,
    evaluations => SpecieEvaluations,
    time_stamp => TimeStamp,
    validation_fitness => {ValFitness, ChampionId}
  }),
  Stats = models:get(stats, S),
  UStats = [Stat | Stats],
  db:write(models:set({stats, UStats}, S), specie),
  Stat.

validation_testing(SpecieId, State) ->
  OpModes = State#p_state.op_modes,
  case lists:member(validation, OpModes) of
    true ->
      S = db:read(SpecieId, specie),
      SHOF = models:get(hall_of_fame, S),
      USHOF = champion_val_test(SHOF, []),
      db:write(models:set({hall_of_fame, USHOF}, S), specie),
      SortedChampions = lists:reverse(lists:sort([{models:get(main_fitness, C), models:get(id, C)} || C <- USHOF])),
      logr:info({population_mgr, status, ok, "validation_testing champions", [SortedChampions]}),
      case SortedChampions of
        [{_ChampTrnFitness, ChampionId} | _] ->
          [Champion] = [Champ || Champ <- USHOF, models:get(id, Champ) == ChampionId],
          {models:get(validation_fitness, Champion), ChampionId};
        [] ->
          {[], void}
      end;
    false ->
      {[], void}
  end.

champion_val_test([C | Champions], Acc) ->
  ChampionId = models:get(id, C),
  ValFitness = case models:get(validation_fitness, C) of
    undefined ->
      agent_mgr_client:start_agent(ChampionId, validation),
      receive
        {handle, {validation_complete, _AgentId, Fitness}} ->
          Fitness
        after 60000 ->
          {[], void}
      end;
    Fitness ->
      Fitness
  end,
  UC = models:set({validation_fitness, ValFitness}, C),
  champion_val_test(Champions, [UC | Acc]);
champion_val_test([], Acc) ->
  lists:reverse(Acc).

calculate_specie_avg_nodes(SpecieId) ->
  AgentIds = select_agents_by_specie(SpecieId),
  calculate_avg_nodes(AgentIds, []).

calculate_specie_diversity(SpecieId) ->
  AgentIds = select_agents_by_specie(SpecieId),
  calculate_diversity(AgentIds).

calculate_fitness(AgentIds) ->
  calculate_fitness(AgentIds, []).

calculate_fitness([AgentId | AgentIds], FitnessAcc) ->
  A = db:read(AgentId, agent),
  case models:get(fitness, A) of
    undefined ->
      calculate_fitness(AgentIds, FitnessAcc);
    Fitness ->
      calculate_fitness(AgentIds, [Fitness | FitnessAcc])
  end;
calculate_fitness([], FitnessAcc) ->
  FitnessAcc.

calculate_avg_nodes([AgentId | AgentIds], NAcc) ->
  A = db:read(AgentId, agent),
  Cx = db:read(models:get(cx_id, A), cortex),
  TotNeurons = length(models:get(neuron_ids, Cx))/1,
  calculate_avg_nodes(AgentIds, [TotNeurons | NAcc]);
calculate_avg_nodes([], NAcc) ->
  {functions:avg(NAcc), functions:std(NAcc)}.

calculate_diversity(AgentIds) ->
  calculate_diversity(AgentIds, []).

calculate_diversity([AgentId | AgentIds], DiversityAcc) ->
  A = db:read(AgentId, agent),
  Fingerprint = models:get(fingerprint, A),
  UDiversityAcc = (DiversityAcc -- [Fingerprint]) ++ [Fingerprint],
  calculate_diversity(AgentIds, UDiversityAcc);
calculate_diversity([], DiversityAcc) ->
  length(DiversityAcc).

vector_basic_stats(VectorList) ->
  TVectorList = transpose(VectorList, [], [], []), % Does not retain order
  [VecSample|_TVL] = TVectorList,
  Length = length(VecSample),
  AvgVector = [lists:sum(V) / Length || V <- TVectorList],
  StdVector = std_vector(TVectorList, AvgVector, []),
  MaxVector = lists:max(VectorList),
  MinVector = lists:min(VectorList),
  {MaxVector, MinVector, AvgVector, StdVector}.

transpose([V | VectorList], RemAcc, ValAcc, VecAcc) ->
  case V of
    [] ->
      lists:reverse(VecAcc);
    [Val | Rem] ->
      transpose(VectorList, [Rem | RemAcc], [Val | ValAcc], VecAcc);
    Other ->
      transpose(VectorList, RemAcc, [Other | ValAcc], VecAcc)
  end;
transpose([], RemAcc, ValAcc, VecAcc) ->
  transpose(RemAcc, [], [], [ValAcc | VecAcc]).

std_vector([List | TVectorList], [Avg | AvgVector], Acc) ->
  std_vector(TVectorList, AvgVector, [functions:std(List, Avg, []) | Acc]);
std_vector([], [], Acc) ->
  lists:reverse(Acc).

summon_agents() ->
  ets:foldl(fun({K, V, _}, ok) -> start_agent({V, K}) end, ok, active_agents),
  ok.

start_agent(AgentId) ->
  agent_mgr_client:start_agent(AgentId, gt),
  ok.

start_agent(AgentId, OpMode) ->
  agent_mgr_client:start_agent(AgentId, OpMode).

stop_agent(AgentId) ->
  agent_mgr_client:stop_agent(AgentId).

update_population_status(PopulationId, PopGen) ->
  PS = ets:lookup_element(population_status, PopulationId, 2),
  UPS = models:set([{pop_gen, PopGen}], PS),
  ets:insert(population_status, {PopulationId, UPS}).

fitness_goal_reached(_BestFitness, inf) ->
  false;
fitness_goal_reached(BestFitness, FitnessGoal) ->
  case BestFitness > FitnessGoal of
    true ->
      true;
    false ->
      false
  end.

intrapopulation_selection(PopulationId, SpecieSizeLim, SelectionAlgorithm) ->
  P = db:read(PopulationId, population),
  SpecieIds = models:get(specie_ids, P),
  [intraspecie_selection(SpecieId, SpecieSizeLim, SelectionAlgorithm) || SpecieId <- SpecieIds], ok.

intraspecie_selection(SpecieId, SpecieSizeLim, SelectionAlgorithmName) ->
  S = db:read(SpecieId, specie),
  Distinguishers = models:get(hof_distinguishers, S),
  AgentIds = select_agents_by_specie(SpecieId),
  SHOF = models:get(hall_of_fame, S),
  {USHOF, Losers} = update_shof(SHOF, AgentIds, Distinguishers, []),
  {AvgFitness, Std, MaxFitness, MinFitness} = calculate_specie_fitness(SpecieId),
  {Factor, Fitness} = models:get(innovation_factor, S),
  UInnovationFactor = case MaxFitness > Fitness of
    true ->
      {0, MaxFitness};
    false ->
      {Factor - 1, Fitness}
  end,
  US = models:set([{hall_of_fame, USHOF}, {fitness, {AvgFitness, Std, MaxFitness, MinFitness}}, {innovation_factor, UInnovationFactor}], S),
  db:write(US, specie),
  selection_algorithm:SelectionAlgorithmName(SpecieId, Losers, SpecieSizeLim).

select_agents_by_specie(SpecieId) ->
  SelectSpec = ets:fun2ms(fun({UId, agent, SId}) when SId == SpecieId -> {agent, UId} end),
  ActiveAgents = ets:select(active_agents, SelectSpec),
  InactiveAgents = ets:select(inactive_agents, SelectSpec),
  lists:usort((ActiveAgents ++ InactiveAgents)).

update_shof(SHOF, [AgentId | AgentIds], Distinguishers, Acc) ->
  case update_shof(SHOF, AgentId, Distinguishers) of
    {USHOF, undefined} ->
      update_shof(USHOF, AgentIds, Distinguishers, Acc);
    {USHOF, Loser} ->
      update_shof(USHOF, AgentIds, Distinguishers, [Loser | Acc])
  end;
update_shof(SHOF, [], _Distinguishers, Acc) ->
  {SHOF, Acc}.

update_shof(SHOF, AgentId, Distinguishers) ->
  Agent = to_champion_form(SHOF, AgentId, Distinguishers),
  FS = app_config:get_env(fitness_stagnation),
  case [C || C <- SHOF, models:get(hof_fingerprint, Agent) == models:get(hof_fingerprint, C)] of
    [] ->
      % Champion with such fingerprint does not exist, thus it is entered, as a
      % stepping stone, into the HOF
      A = db:read(models:get(id, Agent), agent),
      UA = models:set({champion_flag, [true | models:get(champion_flag, A)]}, A),
      db:write(UA, agent),
      update_fitness_stagnation(models:get(id, Agent), better, FS),
      {[Agent | SHOF], undefined};
    Champs ->
      % Agents with this fingerprint exist, and new agent is either entered or
      % not into HOF based on fitness dominance... or behavioral minimal
      % difference.
      SHOFRemainder = SHOF -- Champs,
      case fitness_domination(Agent, Champs) of
        false ->
          update_fitness_stagnation(models:get(id, Agent), worse, FS),
          {SHOF, Agent};
        UChamps ->
          update_fitness_stagnation(models:get(id, Agent), better, FS),
          {SHOFRemainder ++ UChamps, undefined}
      end
  end.

update_fitness_stagnation(_, _, false) ->
  ok;
update_fitness_stagnation(Id, worse, true) ->
  A = db:read(Id, agent),
  case models:get(parent_ids, A) of
    [AncestorId] ->
      Ancestor = db:read(AncestorId, agent),
      FS = models:get(fs, Ancestor),
      logr:debug({population_mgr, update_fitness_stagnation, ok, "FS worse", [{FS, AncestorId}]}),
      db:write(models:set({fs, (FS - FS * 0.1)}, Ancestor), agent);
    [] ->
      ok
  end;
update_fitness_stagnation(Id, better, true) ->
  A = db:read(Id, agent),
  case models:get(parent_ids, A) of
    [AncestorId] ->
      Ancestor = db:read(AncestorId, agent),
      FS = models:get(fs, Ancestor),
      logr:debug({population_mgr, update_fitness_stagnation, ok, "FS better", [{FS, AncestorId}]}),
      db:write(models:set({fs, (FS + (1 - FS) * 0.1)}, Ancestor), agent);
    [] ->
      ok
  end.

fitness_domination(Agent, SHOF) ->
  case fitness_domination(Agent, SHOF, [], []) of
    dominated ->
      false;
    {on_pareto, RemainingChamps} ->
      A = db:read(models:get(id, Agent), agent),
      UA = models:set({champion_flag, [true | models:get(champion_flag, A)]}, A),
      db:write(UA, agent),
      [Agent | RemainingChamps];
    dominating->
      A = db:read(models:get(id, Agent), agent),
      UA = models:set({champion_flag, [true | models:get(champion_flag, A)]}, A),
      db:write(UA, agent),
      [Agent];
    {strange, _LoserAcc, RemainingChamps} ->
      logr:warning({population_mgr, fitness_domination, error, "algorithmic error", []}),
      A = db:read(models:get(id, Agent), agent),
      UA = models:set({champion_flag, [true | models:get(champion_flag, A)]}, A),
      db:write(UA, agent),
      [Agent | RemainingChamps]
  end.

fitness_domination(Agent, [Champ | Champs], LoserAcc, Acc) ->
  case models:get(hof_fingerprint, Agent) == models:get(hof_fingerprint, Champ) of
    true ->
      VecDif = utils:vec1_dominates_vec2(models:get(fitness, Agent), models:get(fitness, Champ), 0.0, []),
      TotDomElems = length([Val || Val <- VecDif, Val > 0]),
      TotElems = length(VecDif),
      case TotDomElems of
        TotElems ->
          ChampA = db:read(models:get(id, Champ), agent),
          UChampA = models:set({champion_flag, [lost | models:get(champion_flag, ChampA)]}, ChampA),
          db:write(UChampA, agent),
          fitness_domination(Agent, Champs, [Champ | LoserAcc], Acc);
        0 ->
          dominated;
        _ ->
          fitness_domination(Agent, Champs, LoserAcc, [Champ | Acc])
      end;
    false ->
      fitness_domination(Agent, Champs, LoserAcc, [Champ | Acc])
  end;
fitness_domination(_Agent, [], _LoserAcc, []) ->
  dominating;
fitness_domination(_Agent, [], [], Acc) ->
  {on_pareto, Acc};
fitness_domination(_Agent, [], LoserAcc, Acc) ->
  {strange, LoserAcc, Acc}.

to_champion_form(_SHOF, AgentId, Distinguishers) ->
  A = db:read(AgentId, agent),
  models:champion(#{
    hof_fingerprint => [specie_identifier:D(AgentId) || D <- Distinguishers],
    id => AgentId,
    fitness => models:get(fitness, A),
    validation_fitness => undefined,
    main_fitness => models:get(main_fitness, A),
    tot_n => length(lists:flatten([NIds || {_LayerId, NIds} <- models:get(pattern, A)])),
    generation => models:get(generation, A),
    fs => models:get(fs, A)
  }).
