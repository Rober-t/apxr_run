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
%%% @doc The experiment_mgr process sequentially spawns population_mgrs and
%%%      applies them to some specified problem. The experiment_mgr can compose
%%%      experiments by performing multiple evolutionary runs, and then
%%%      produce statistical data and graph ready ready files of the various
%%%      statistics calculated from the experiment.
%%%      Specifically, it has three main functionalities:
%%%       (1) Run the population_mgr N number of times, waiting for the
%%%           population_mgr's trace after every run.
%%%       (2) Create the experiment entry in the database, and keep
%%%           updating its trace_acc as it itself accumulates the traces from
%%%           spawned population_mgrs. The population_mgr should only do this
%%%           if the backup_flag is set to true in the experiment record with
%%%           which the experiment was started.
%%%       (3) When the experiment_mgr has finished performing N number of
%%%           evolutionary runs, and has accumulated N number of traces, it
%%%           prints all the traces to console, calculates averages of the
%%%           parameters between all the traces, and then finally write that
%%%           data to file in the format which can be immediately graphed.
%%%      In addition, because the experiment_mgr might be interrupted as it
%%%      accumulates the traces, the init function checks if an experiment
%%%      with given unique Id already exists and if it does, has it been
%%%      completed yet. If the experiment has not been completed, it reads from
%%%      the database all the needed the information about the experiment, and
%%%      then runs the population_mgr the remaining number of times to complete
%%%      the whole experiment.
%%% @end
%%%----------------------------------------------------------------------------
-module(experiment_mgr).

-behaviour(gen_server).

%% Start/Stop
-export([
  start_link/0
]).

%% API
-export([
  run/0,
  complete/2
]).

%% gen_server callbacks
-export([
  init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2
]).

%% Xref
-ignore_xref([
  start_link/0,
  complete/1
]).

%%%============================================================================
%%% Types
%%%============================================================================

-record(exp_mgr_state, {
  exp :: models:experiment() | undefined,
  population_id :: models:population_id() | undefined
}).

-record(graph, {
  morphology :: atom() | {atom(), atom()},
  avg_neurons = [] :: float() | [float()],
  neurons_std = [] :: float() | [float()],
  avg_fitness = [] :: [[float()]],
  fitness_std = [] :: [[float()]],
  max_fitness = [] :: [[float()]],
  min_fitness = [] :: [[float()]],
  maxavg_fitness = [] :: [[float()]],
  maxavg_fitness_std = [] :: [[float()]],
  minavg_fitness = [] :: [[float()]],
  avg_diversity = [] :: float() | [float()],
  diversity_std = [] :: float() | [float()],
  evaluations = [] :: float() | [float()],
  validation_fitness = [] :: [[float()]],
  validation_fitness_std = [] :: [[float()]],
  validationmax_fitness = [] :: [[float()]],
  validationmin_fitness = [] :: [[float()]],
  evaluation_index = [] :: [integer()]
}).

-record(avg, {
  avg_neurons = [] :: float() | [float()],
  neurons_std = [] :: float() | [float()],
  avg_fitness = [] :: [[float()]],
  fitness_std = [] :: [[float()]],
  max_fitness = [] :: [[float()]],
  min_fitness = [] :: [[float()]],
  maxavg_fitness :: [[float()]] | undefined,
  maxavg_fitness_std = [] :: [[float()]],
  minavg_fitness :: [[float()]] | undefined,
  avg_diversity = [] :: float() | [float()],
  diversity_std = [] :: float() | [float()],
  evaluations = [] :: float() | [float()],
  validation_fitness = [] :: [[float()]],
  validation_fitness_std = [] :: [[float()]],
  validationmax_fitness = [] :: [[float()]],
  validationmin_fitness = [] :: [[float()]]
}).

-type state() :: #exp_mgr_state{}.

%%%============================================================================
%%% API
%%%============================================================================

%%-----------------------------------------------------------------------------
%% @doc The start_link function starts the experiment_mgr server with the given
%%      parameters and options.
%% @end
%%-----------------------------------------------------------------------------
-spec start_link() -> {ok, pid()}.
start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%-----------------------------------------------------------------------------
%% @doc The run function first checks whether the experiment_mgr process is
%%      online. If the experiment_mgr is online, the run function triggers a
%%      new experiment by starting the population_mgr, otherwise, it returns an
%%      error.
%% @end
%%-----------------------------------------------------------------------------
-spec run() -> ok.
run() ->
  case whereis(experiment_mgr) of
    undefined ->
      logr:error({experiment_mgr, run, error, "ExperimentMgr cannot run, it is not online", []}),
      {error, "ExperimentMgr not online"};
    Pid ->
      gen_server:cast(Pid, {run})
  end.

%%-----------------------------------------------------------------------------
%% @doc The complete function checks the current run index vs total_runs. If the
%%      experiment has completed the set number of runs then the experiment
%%      record is marked as completed and a report is generated. If not,
%%      another run is triggered by spawning a new population_mgr.
%% @end
%%-----------------------------------------------------------------------------
-spec complete(models:population_id(), populaton_mgr:trace()) -> ok.
complete(PopulationId, Trace) ->
  gen_server:cast(?MODULE, {complete, PopulationId, Trace}).

%%%============================================================================
%%% gen_server callbacks
%%%============================================================================

%%-----------------------------------------------------------------------------
%% @private
%% @doc Initializes the experiment_mgr server.
%% @end
%%-----------------------------------------------------------------------------
-spec init([]) -> {ok, state()}.
init([]) ->
  process_flag(trap_exit, true),
  logr:debug({experiment_mgr, init, ok, undefined, []}),
  {ok, #exp_mgr_state{}}.

%%-----------------------------------------------------------------------------
%% @private
%% @doc Handle call messages.
%% @end
%%-----------------------------------------------------------------------------
-spec handle_call(term(), {pid(), term()}, state()) -> {reply, ok, state()}.
handle_call(Request, From, State) ->
  logr:warning({experiment_mgr, msg, error, "unexpected call", [Request, From]}),
  {reply, ok, State}.

%%-----------------------------------------------------------------------------
%% @private
%% @doc Handle cast messages.
%% @end
%%-----------------------------------------------------------------------------
-spec handle_cast({run} | {complete, models:population_id(), populaton_mgr:trace()}, state()) ->
{noreply, state()}.
handle_cast({run}, State) ->
  NewState = do_run_setup(State),
  population_mgr_client:new_run(),
  {noreply, NewState};

handle_cast({complete, _PopId, Trace}, S) ->
  {ok, NewState} = do_complete(Trace, S),
  {noreply, NewState}.

%%-----------------------------------------------------------------------------
%% @private
%% @doc Handle info messages.
%% @end
%%-----------------------------------------------------------------------------
-spec handle_info(any(), state()) -> {noreply, state()}.
handle_info(Info, State) ->
  case Info of
    {'EXIT', _Pid, normal} ->
      {noreply, State};
    {'EXIT', Pid, shutdown} ->
      logr:debug({experiment_mgr, msg, ok, "shutdown message", [Pid]}),
      {stop, shutdown, State};
    {'EXIT', Pid, Reason} ->
      logr:debug({experiment_mgr, msg, ok, "exit message", [Pid, Reason]}),
      {stop, Reason, State};
    UnexpectedMsg ->
      logr:warning({experiment_mgr, msg, error, "unexpected info message", [UnexpectedMsg]}),
      {noreply, State}
  end.

%%-----------------------------------------------------------------------------
%% @private
%% @doc When the experiment_mgr process terminates, it states so,
%%      notifies with what op_tag it terminated with, all the states
%%      gathered, and then shuts down.
%% @end
%%-----------------------------------------------------------------------------
-spec terminate(any(), state()) -> ok.
terminate(Reason, _S) ->
  logr:info({experiment_mgr, status, ok, "experiment_mgr terminated", [Reason]}),
  ok.

%%%============================================================================
%%% Internal functions
%%%============================================================================

do_run_setup(S) ->
  Id = app_config:get_env(identifier),
  PMP = app_config:get_env(pmp),
  Constraints = app_config:get_env(constraints),
  Runs = app_config:get_env(runs),
  E = case db:read(Id, experiment) of
    not_found ->
      build_model(Id, PMP, Constraints, Runs);
    Prev ->
      case models:get(progress_flag, Prev) of
        completed ->
          logr:error({experiment_mgr, do_run_setup, error, "experiment complete", [Id]}),
          polis_mgr:stop();
        in_progress ->
          logr:info({experiment_mgr, status, ok, "previously started. continuing", [Id]}),
          Interruptions = models:get(interruptions, Prev),
          UInterruptions =
            [calendar:system_time_to_rfc3339(erlang:system_time(second)) | Interruptions],
          models:set({interruptions, UInterruptions}, Prev)
      end
  end,
  db:write(E, experiment),
  PopulationId = models:get(population_id, PMP),
  logr:info({experiment_mgr, status, ok, "run", [PMP, Constraints]}),
  NewState = S#exp_mgr_state{exp = E, population_id = PopulationId},
  NewState.

do_complete(Trace, S) ->
  E = S#exp_mgr_state.exp,
  UTraceAcc = [Trace | models:get(trace_acc, E)],
  URunIndex = models:get(run_index, E) + 1,
  case URunIndex > models:get(total_runs, E) of
    true ->
      logr:info({experiment_mgr, status, ok, "experiment complete", [models:get(run_index, E)]}),
      do_report(E, UTraceAcc, URunIndex),
      timer:sleep(45000),
      polis_mgr:backup_and_shutdown(),
      {ok, S};
    false ->
      UE = do_restart(E, UTraceAcc, URunIndex),
      NewState = S#exp_mgr_state{exp = UE},
      {ok, NewState}
  end.

do_report(E, UTraceAcc, URunIndex) ->
  UE = models:set([{trace_acc, UTraceAcc}, {run_index, URunIndex},
    {completed, {
      calendar:system_time_to_rfc3339(erlang:system_time(second)),
      erlang:monotonic_time()}}, {progress_flag, completed}], E),
  db:write(UE, experiment),
  report(models:get(id, UE), "report"),
  UE.

do_restart(E, UTraceAcc, URunIndex) ->
  UE = models:set([{trace_acc, UTraceAcc}, {run_index, URunIndex}], E),
  db:write(UE, experiment),
  logr:info({experiment_mgr, status, ok, "experiment_complete. restarting",
    [models:get(run_index, E)]}),
  population_mgr_client:restart_run(),
  UE.

build_model(Id, PMP, Constraints, Runs) ->
  models:experiment(#{
    id => Id,
    backup_flag => true,
    pm_parameters => PMP,
    init_constraints => Constraints,
    progress_flag => in_progress,
    trace_acc => [],
    run_index => 1,
    total_runs => Runs,
    started => {
      calendar:system_time_to_rfc3339(erlang:system_time(second)),
      erlang:monotonic_time()
    },
    completed => {},
    interruptions => []
  }).

report(ExperimentId, FileName) ->
  E = db:read(ExperimentId, experiment),
  {ok, EFile} = file:open("experiments/" ++ FileName ++ "_experiment", [write]),
  io:format(EFile, "~p", [E]),
  file:close(EFile),
  Traces = models:get(trace_acc, E),
  {ok, File} = file:open("experiments/" ++ FileName ++ "_trace", [write]),
  lists:foreach(fun(X) -> io:format(File, "~p.~n", [X]) end, Traces),
  file:close(File),
  Graphs = prepare_graphs(Traces),
  write_graphs(Graphs).

%%-----------------------------------------------------------------------------
%% @private
%% @doc Each trace is composed of a list of lists of stats. Length of list of
%%      stats determines the number of species...we need to graph that so
%%      that we can graph the features against evaluations.
%%       1. Separate into Traces
%%       2. Separate Traces into stats
%%       3. Extract from each stats the various features against evaluations
%%       4. Combine the whatever from all stats from all traces into the
%%          averages.
%% @end
%%-----------------------------------------------------------------------------
prepare_graphs(Traces) ->
  [T | _] = Traces,
  [StatsList | _] = models:get(stats, T),
  Morphologies = [models:get(morphology, S) || S <- StatsList],
  MorphologyGraphs = [prep_traces(Traces, Morphology, []) || Morphology <- Morphologies],
  MorphologyGraphs.

write_graphs([G | Graphs]) ->
  M = G#graph.morphology,
  UG = G#graph{evaluation_index = [500 * Ix || Ix <- lists:seq(1, length(G#graph.avg_fitness))]},
  {ok, File} = file:open("experiments/" ++ "graphs_" ++ atom_to_list(M), [write]),

  io:format(File, "#Avg Fitness Vs Evaluations, Morphology: ~p", [M]),
  print_multi_objective_fitness(File, UG#graph.evaluation_index, UG#graph.avg_fitness,
    UG#graph.fitness_std),

  io:format(File, "~n~n~n#Avg Neurons Vs Evaluations, Morphology: ~p~n", [M]),
  lists:foreach(fun({X, Y, Std}) -> io:format(File, "~p ~p ~p~n", [X, Y, Std]) end,
    lists:zip3(UG#graph.evaluation_index, UG#graph.avg_neurons, UG#graph.neurons_std)),

  io:format(File, "~n~n#Avg Diversity Vs Evaluations, Morphology: ~p~n", [M]),
  lists:foreach(fun({X, Y, Std}) -> io:format(File, "~p ~p ~p~n", [X, Y, Std]) end,
    lists:zip3(UG#graph.evaluation_index, UG#graph.avg_diversity, UG#graph.diversity_std)),

  io:format(File, "~n~n#Max Fitness Vs Evaluations, Morphology: ~p", [M]),
  print_multi_objective_fitness(File, UG#graph.evaluation_index, UG#graph.max_fitness),

  io:format(File, "~n~n~n#Avg. Max Fitness Vs Evaluations, Morphology: ~p", [M]),
  print_multi_objective_fitness(File, UG#graph.evaluation_index, UG#graph.maxavg_fitness),

  io:format(File, "~n~n~n#Avg. Min Fitness Vs Evaluations, Morphology: ~p", [M]),
  print_multi_objective_fitness(File, UG#graph.evaluation_index, UG#graph.min_fitness),

  io:format(File, "~n~n~n#Specie-Population Turnover Vs Evaluations, Morphology: ~p~n", [M]),
  lists:foreach(fun({X, Y}) -> io:format(File, "~p ~p~n", [X, Y]) end,
    lists:zip(UG#graph.evaluation_index, UG#graph.evaluations)),

  io:format(File, "~n~n#Validation Avg Fitness Vs Evaluations, Morphology:~p", [M]),
  print_multi_objective_fitness(File, UG#graph.evaluation_index, UG#graph.validation_fitness,
    UG#graph.validation_fitness_std),

  io:format(File, "~n~n~n#Validation Max Fitness Vs Evaluations, Morphology:~p", [M]),
  print_multi_objective_fitness(File, UG#graph.evaluation_index, UG#graph.validationmax_fitness),

  io:format(File, "~n~n~n#Validation Min Fitness Vs Evaluations, Morphology:~p", [M]),
  print_multi_objective_fitness(File, UG#graph.evaluation_index, UG#graph.validationmin_fitness),

  file:close(File),
  write_graphs(Graphs);
write_graphs([]) ->
  ok.

prep_traces([T | Traces], Morphology, Acc) ->
  MorphologyTrace = lists:flatten([[S || S <- Stats, models:get(morphology, S) == Morphology] ||
    Stats <- models:get(stats, T)]),
  prep_traces(Traces, Morphology, [MorphologyTrace | Acc]);
prep_traces([], Morphology, Acc) ->
  Graph = avg_morphological_traces(lists:reverse(Acc), [], [], []),
  Graph#graph{morphology = Morphology}.

avg_morphological_traces([SList | SLists], Acc1, Acc2, Acc3) ->
  case SList of
    [S | STail] ->
      avg_morphological_traces(SLists, [STail | Acc1], [S | Acc2], Acc3);
    [] ->
    Graph = avg_stats_lists(Acc3, #graph{}),
      Graph
  end;
avg_morphological_traces([], Acc1, Acc2, Acc3) ->
  avg_morphological_traces(lists:reverse(Acc1), [], [], [lists:reverse(Acc2) | Acc3]).

avg_stats_lists([SList | SLists], Graph) ->
  Avg = avg_stats(SList, #avg{}),
  UGraph = Graph#graph{
    avg_neurons = [Avg#avg.avg_neurons | Graph#graph.avg_neurons],
    neurons_std = [Avg#avg.neurons_std | Graph#graph.neurons_std],
    avg_fitness = [Avg#avg.avg_fitness | Graph#graph.avg_fitness],
    fitness_std = [Avg#avg.fitness_std | Graph#graph.fitness_std],
    max_fitness = [Avg#avg.max_fitness | Graph#graph.max_fitness],
    min_fitness = [Avg#avg.min_fitness | Graph#graph.min_fitness],
    maxavg_fitness = [Avg#avg.maxavg_fitness | Graph#graph.maxavg_fitness],
    maxavg_fitness_std = [Avg#avg.maxavg_fitness_std | Graph#graph.maxavg_fitness_std],
    minavg_fitness = [Avg#avg.minavg_fitness | Graph#graph.minavg_fitness],
    evaluations = [Avg#avg.evaluations | Graph#graph.evaluations],
    validation_fitness = [Avg#avg.validation_fitness | Graph#graph.validation_fitness],
    validation_fitness_std = [Avg#avg.validation_fitness_std | Graph#graph.validation_fitness_std],
    validationmax_fitness = [Avg#avg.validationmax_fitness | Graph#graph.validationmax_fitness],
    validationmin_fitness = [Avg#avg.validationmin_fitness | Graph#graph.validationmin_fitness],
    avg_diversity = [Avg#avg.avg_diversity | Graph#graph.avg_diversity],
    diversity_std = [Avg#avg.diversity_std | Graph#graph.diversity_std]
  },
  avg_stats_lists(SLists, UGraph);
avg_stats_lists([], Graph) ->
  Graph#graph{
    avg_neurons = lists:reverse(Graph#graph.avg_neurons),
    neurons_std = lists:reverse(Graph#graph.neurons_std),
    avg_fitness = lists:reverse(Graph#graph.avg_fitness),
    fitness_std = lists:reverse(Graph#graph.fitness_std),
    max_fitness = lists:reverse(Graph#graph.max_fitness),
    min_fitness = lists:reverse(Graph#graph.min_fitness),
    maxavg_fitness = lists:reverse(Graph#graph.maxavg_fitness),
    maxavg_fitness_std = lists:reverse(Graph#graph.maxavg_fitness_std),
    minavg_fitness = lists:reverse(Graph#graph.minavg_fitness),
    validation_fitness = lists:reverse(Graph#graph.validation_fitness),
    validation_fitness_std = lists:reverse(Graph#graph.validation_fitness_std),
    validationmax_fitness = lists:reverse(Graph#graph.validationmax_fitness),
    validationmin_fitness = lists:reverse(Graph#graph.validationmin_fitness),
    evaluations = lists:reverse(Graph#graph.evaluations),
    avg_diversity = lists:reverse(Graph#graph.avg_diversity),
    diversity_std = lists:reverse(Graph#graph.diversity_std)
  }.

avg_stats([S | STail], Avg) ->
  {ValidationFitness, _ChampionId} = models:get(validation_fitness, S),
  UAvg = Avg#avg{
    avg_neurons = [models:get(avg_neurons, S) | Avg#avg.avg_neurons],
    avg_fitness = list_append(models:get(avg_fitness, S), Avg#avg.avg_fitness),
    max_fitness = list_append(models:get(max_fitness, S), Avg#avg.max_fitness),
    min_fitness = list_append(models:get(min_fitness, S), Avg#avg.min_fitness),
    evaluations = [models:get(evaluations, S) | Avg#avg.evaluations],
    validation_fitness = list_append(ValidationFitness, Avg#avg.validation_fitness),
    avg_diversity = [models:get(avg_diversity, S) | Avg#avg.avg_diversity]
  },
  avg_stats(STail, UAvg);
avg_stats([], Avg) ->
  Avg#avg{
    avg_neurons = functions:avg(Avg#avg.avg_neurons),
    neurons_std = functions:std(Avg#avg.avg_neurons),
    avg_fitness = [functions:avg(Val) || Val <- Avg#avg.avg_fitness],
    fitness_std = [functions:std(Val) || Val <- Avg#avg.avg_fitness],
    max_fitness = [lists:max(Val) || Val <- Avg#avg.max_fitness],
    min_fitness = [lists:min(Val) || Val <- Avg#avg.min_fitness],
    maxavg_fitness = [functions:avg(Val) || Val <- Avg#avg.max_fitness],
    maxavg_fitness_std = [functions:std(Val) || Val <- Avg#avg.max_fitness],
    minavg_fitness = [functions:avg(Val) || Val <- Avg#avg.min_fitness],
    evaluations = functions:avg(Avg#avg.evaluations),
    validation_fitness = [functions:avg(Val) || Val <- Avg#avg.validation_fitness],
    validation_fitness_std = [functions:std(Val)||Val <- Avg#avg.validation_fitness],
    validationmax_fitness = [lists:max(Val) || Val <- Avg#avg.validation_fitness],
    validationmin_fitness = [lists:min(Val) || Val <- Avg#avg.validation_fitness],
    avg_diversity = functions:avg(Avg#avg.avg_diversity),
    diversity_std = functions:std(Avg#avg.avg_diversity)
}.

list_append([], []) ->
  [];
list_append(ListA, []) ->
  [[Val] || Val <- ListA];
list_append([], ListB) ->
  ListB;
list_append(ListA, ListB) ->
  list_append(ListA, ListB, []).

list_append([Val | ListA], [AccB | ListB], Acc) ->
  list_append(ListA, ListB, [[Val | AccB] | Acc]);
list_append(undefined, [AccB | ListB], Acc) ->
  list_append([], ListB, [AccB | Acc]);
list_append([], [], Acc) ->
  lists:reverse(Acc).

print_multi_objective_fitness(File, [I | Index], [F | Fitness], [Std | StandardDiviation]) ->
  case (F == []) or (Std == []) of
    true ->
      ok;
    false ->
      io:format(File, "~n~p ", [I]),
      print_fitness_and_std(File, F, Std)
  end,
  print_multi_objective_fitness(File, Index, Fitness, StandardDiviation);
print_multi_objective_fitness(_File, [], [], []) ->
  ok.

print_fitness_and_std(File, [FE | FitnessElements], [SE | StdElements]) ->
  io:format(File, "~p ~p", [FE, SE]),
  print_fitness_and_std(File, FitnessElements, StdElements);
print_fitness_and_std(_File, [], []) ->
  ok.

print_multi_objective_fitness(File, [I | Index], [F | Fitness]) ->
  case F of
    [] ->
      ok;
    _ ->
      io:format(File, "~n~p", [I]),
      [io:format(File, " ~p", [FE]) || FE <- F]
  end,
  print_multi_objective_fitness(File, Index, Fitness);
print_multi_objective_fitness(_File, [], []) ->
  ok.
