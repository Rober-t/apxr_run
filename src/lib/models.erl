%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Copyright (C) 2018 ApproximateReality
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%----------------------------------------------------------------------------
%%% @doc Shared data models and functions
%%% @end
%%%----------------------------------------------------------------------------
-module(models).

%% API
-export([
  get/2,
  set/2
]).

%% Models
-export([
  topology_summary/1,
  sensor/1,
  actuator/1,
  neuron/1,
  cortex/1,
  substrate/1,
  constraint/1,
  experiment/1,
  agent/1,
  champion/1,
  pmp/1,
  stat/1,
  trace/1,
  population/1,
  population_status/1,
  specie/1
]).

%% Xref
-ignore_xref([
  get/2,
  set/2,
  topology_summary/1,
  sensor/1,
  actuator/1,
  neuron/1,
  cortex/1,
  substrate/1,
  constraint/1,
  experiment/1,
  agent/1,
  champion/1,
  pmp/1,
  stat/1,
  trace/1,
  population/1,
  population_status/1,
  specie/1
]).

%%=============================================================================
%% Types
%%=============================================================================

-type population_id() :: {population, float()}.
-type specie_id() :: {specie, float()}.
-type scape_id() :: {public | private , atom()}.
-type agent_id() :: {agent, float()}.
-type cortex_id() :: {cortex, {origin, float()}}.
-type actuator_id() :: {actuator, {float(), float()}}.
-type actuator_name() :: {atom(), atom()} |
  delta_weight | set_abcn | set_weight | weight_expression.
-type sensor_id() :: {sensor, {float(), float()}}.
-type sensor_name() :: {atom(), atom()} |
  cartesian | centripital_distances | cartesian_distance | cartesian_coord_diffs |
  cartesian_gaussed_coord_diffs | iow | polar | spherical.
-type neuron_id() :: {neuron, {float(), float()}}.
-type substrate_id() :: {substrate, {void, float()}}.

-type op_mode() :: gt | validation.

-type encoding_type() :: neural | substrate.
-type format() :: no_geo | geo |
  {coorded, integer(), [integer()], float()} | {symmetric, [integer()]}.
-type linkform() :: l2l_feedforward | jordan_recurrent |
  fully_connected | fully_interconnected | neuronself_recurrent.

-type neural_af() :: tanh | relu | cos | gaussian | absolute | sin | sqrt | sigmoid.
-type neural_pfn() ::
  none | hebbian_w | hebbian | ojas_w | ojas |
  self_modulation_v1 | self_modulation_v2 |
  self_modulation_v2 | self_modulation_v3 |
  self_modulation_v4 | self_modulation_v5 |
  self_modulation_v6 | neuromodulation.

-type substrate_plasticity() :: none | iterative | abcn.

-type tuning_selection_f() ::
  all | all_random | dynamic | dynamic_random |
  active | active_random | current | current_random.
-type specie_distinguisher() ::
  tot_n | tot_inlinks | tot_outlinks | tot_sensors | tot_actuators |
  pattern | tot_tanh | tot_sin | tot_cos | tot_gaus | tot_lin.
-type hof_distinguisher() ::
  tot_n | tot_inlinks | tot_outlinks | tot_sensors | tot_actuators |
  pattern | tot_tanh | tot_sin | tot_cos | tot_gaus | tot_lin.
-type objective() ::
  main_fitness | problem_specific_fitness |
  other_optimization_factors | inverse_tot_n.

-type fingerprint() :: {
  GeneralizedPattern :: [{float(), integer()}],
  GeneralizedEvoHist :: [{atom(), {float(), atom()}}],
  GeneralizedSensors :: [sensor()],
  GeneralizedActuators :: [actuator()],
  topology_summary()} | origin.

-type evo_hist() :: [{atom(), neuron_id()}] |
  [{atom(), neuron_id(), neuron_id()}] |
  [{atom(), neuron_id(), neuron_id(), neuron_id()}].

-type key() :: term().
-type value() :: term().

-type table() :: topology_summary | sensor | actuator | neuron | cortex | substrate | constraint |
experiment | agent | champion | pmp | stat | trace | population | population_status | specie.

-type model() :: topology_summary() | sensor() | actuator() | neuron() | cortex() | substrate() |
constraint() | experiment() | agent() | champion() | pmp() | stat() | trace() |
population() | population_status() | specie().

-export_type([
  model/0,
  table/0,
  key/0,
  value/0
]).

-export_type([
  population_id/0,
  specie_id/0,
  scape_id/0,
  agent_id/0,
  cortex_id/0,
  actuator_id/0,
  actuator_name/0,
  sensor_id/0,
  sensor_name/0,
  neuron_id/0,
  substrate_id/0,
  op_mode/0,
  encoding_type/0,
  format/0,
  linkform/0,
  neural_af/0,
  neural_pfn/0,
  substrate_plasticity/0,
  tuning_selection_f/0,
  specie_distinguisher/0,
  hof_distinguisher/0,
  objective/0,
  fingerprint/0,
  topology_summary/0,
  evo_hist/0
]).

-export_type([
  sensor/0,
  actuator/0,
  neuron/0,
  cortex/0,
  substrate/0,
  constraint/0,
  experiment/0,
  agent/0,
  champion/0,
  pmp/0,
  stat/0,
  trace/0,
  population/0,
  population_status/0,
  specie/0
]).

%%-----------------------------------------------------------------------------
%% Genotype
%%-----------------------------------------------------------------------------

-type topology_summary() :: #{data := #{
  type := encoding_type(),
  % Is the total number of neurons composing the NN based agent.
  tot_neurons := integer(),
  % Is the total number of neuronal input links, calculated by adding together
  % the number of input links of every neuron in the NN based agent.
  tot_n_ils := integer(),
  % Is the total number of of neuronal output links, counted in the same way as
  % input links. Though somewhat redundant, the tot_n_ils and tot_n_ols
  % will differ from each other based on the number of sensors and actuators
  % used by the agent.
  tot_n_ols := integer(),
  % Is the total number of recurrent connections within the NN system.
  tot_n_ros := integer(),
  % Is the count of every type of activation function used by the NN based
  % agent. This has the format of {TotTanh, TotSin, TotCos, TotGaussian,
  % TotAbsolute, TotSgn, TotLog, TotSqrt, TotLin}, and thus agents which have
  % the same topology, but whose neurons use different sets of activation
  % function, will be functionally very different, and this activation function
  % frequency distribution tuple will to some degree summarize these
  % differences. There could of course be numerous agents with the same
  % topology and the same activation function frequency distribution, but which
  % differ in the neurons which use those activation functions, and locations of
  % of those neurons within the NN topology. Nevertheless, this gives us a short
  % and easy way to calculate a summary which could be used in addition to other
  % fingerprint elements to distinguish between two functionally different
  % agents. These summaries can be further considered as representing how the
  % different agents are exploring the different areas on the topological
  % landscape.
  af_distribution := [{atom(), integer()}]
  % Thus, based on these defining characteristics, two agents with different
  % Fingerprints (which will include their topological summaries), will warrant
  % being considered as significantly different. The diversity is then the total
  % number of different << fingerprints >> within the population.
}}.

%%-----------------------------------------------------------------------------
%% Sensor
%%-----------------------------------------------------------------------------

-type sensor() :: #{data := #{
  id := sensor_id() | undefined,
  name := sensor_name(),
  type := atom(),
  cx_id := cortex_id() | undefined,
  scape := scape_id() | undefined,
  vl := integer(),
  fanout_ids := [neuron_id()] | [substrate_id()] | [],
  generation := integer() | undefined,
  format := format() | undefined,
  parameters := [any()] | undefined
}}.

%%-----------------------------------------------------------------------------
%% Actuator
%%-----------------------------------------------------------------------------

-type actuator() :: #{data := #{
  id := actuator_id() | undefined,
  name := actuator_name(),
  type := atom(),
  cx_id := cortex_id() | undefined,
  scape := scape_id() | undefined,
  vl := integer(),
  fanin_ids := [neuron_id()] | [substrate_id()] | [],
  generation := integer() | undefined,
  format := format() | undefined,
  parameters := [any()] | undefined
}}.

%%-----------------------------------------------------------------------------
%% Neuron
%%-----------------------------------------------------------------------------

-type neuron() :: #{data := #{
  id := neuron_id(),
  generation := integer(),
  cx_id := cortex_id(),
  af := neural_af(),
  % Is an atom, the name of a plasticity function. The plasticity function
  % accepts as input the synaptic weight list, input vector, and output vector.
  % The output of this function is the updated version of the synaptic weights.
  pf := {neural_pfn(), [float()]}, % {PFName, PFParameters}
  % Is an atom, the name of the aggregation function. For example, the dot
  % function, which simply aggregates the input signals, dots them with the
  % synaptic weights, adds the bias if present, and returns the result, which
  % is to be sent through the activation function.
  aggr_f := atom(),
  input_idps := [{models:sensor() | models:neuron_id(), [{float(), [float()] | []}]}] | [],
  input_idps_modulation := [{models:neuron_id(), [{float(), [float()] | []}]}] | [],
  output_ids := [neuron_id() | actuator_id()] | [],
  ro_ids := [neuron_id()] | []
}}.

%%-----------------------------------------------------------------------------
%% Cortex
%%-----------------------------------------------------------------------------

-type cortex() :: #{data := #{
  id := cortex_id(),
  agent_id := agent_id(),
  neuron_ids := [neuron_id()] | [],
  sensor_ids := [sensor_id()] | [],
  actuator_ids := [actuator_id()] | []
}}.

%%-----------------------------------------------------------------------------
%% Substrate
%%-----------------------------------------------------------------------------

-type substrate() :: #{data := #{
  id := substrate_id(),
  agent_id := agent_id(),
  densities := [integer()],
  linkform := linkform(),
  plasticity := none | iterative | abcn,
  cpp_ids := [{atom(), {float(), float()}}] | [],
  cep_ids := [{atom(), {float(), float()}}] | []
}}.

%%-----------------------------------------------------------------------------
%% ExperimentMgr
%%-----------------------------------------------------------------------------

% This particular model is used to initialize the population model, it
% specifies the constraint of the species to which it belongs, and the choices
% and functions the population can use. It is this model that contains the
% initial list of the available functions for each category: activation
% functions, plasticity functions, selection functions...
-type constraint() :: #{data := #{
  morphology := atom(),
  connection_architecture := recurrent | feedforward,
  neural_afs := [neural_af()],
  % Is a list composed of plasticity function names, such as hebbian, ojas, and
  % others that get added over time. When a neuron is created, it randomly
  % chooses a plasticity function to use, similar to the way it chooses an
  % activation function. In the population_mgr the one can choose to use a list
  % composed of a single plasticity type, for example the << none >> function,
  % which means that the neurons of this NN will not have plasticity.
  neural_pfns := [neural_pfn()],
  substrate_plasticities := [substrate_plasticity()],
  substrate_linkforms := [linkform()],
  % Is a list composed of tuning selection function names. The different agents
  % can start of using different tuning selection functions (neurons chosen for
  % tuning during evaluation), allowing the use to determine which of the
  % selection functions is more advantageous in a particular simulation. This is
  % also simply a good way to specify in the population_mgr, when creating a
  % seed population using the SpecCon variable, the selection algorithm that one
  % wishes for the agents in the population to use.
  neural_aggr_fs := [dot_product | diff_product | mult_product],
  tuning_selection_fs := [tuning_selection_f()],
  % Is a tuple composed of the tuning during function name and its parameter.
  % All agents in the population must use the same tuning_duration_f function so
  % that their performance is judged on equal grounds. Thus they are all given
  % the same number of chances to improve during their tuning phase. Also, if we
  % set it to {const, 1}, then we can effectively convert our neuroevolutionary
  % system to a genetic rather than memetic algorithm based TWEANN, since every
  % agent will have the exoself simply perform a single evaluation to get its
  % fitness and then immediately terminate, waiting for the topological mutation
  % phase.
  tuning_duration_f := {atom(), float()},
  % The annealing_parameters can be composed of a single float, or a list of
  % them if one wishes to experiment with a population composed of agents using
  % different perturbation ranges.
  annealing_parameters := [float()],
  % This is a list of floats, each of which specifies the multiplier of
  % math:pi(). The actual perturbation range is: Multiplier * math:pi(). The
  % perturbation_ranges can be composed of a single float, or a list of them if
  % one wishes to experiment with a population composed of agents using
  % different perturbation ranges.
  perturbation_ranges := [float()],
  agent_encoding_types := [neural | substrate],
  heredity_types := [darwinian | lamarckian],
  % Is a list of tuples, composed of function names of the mutation operators
  % available to a particular agent, and the probability of being used, as
  % proportional to other operators. We would usually have a single list for
  % the entire population, or have different populations each with a different
  % set of mutation operator lists available, if for example we wish to test
  % whether a mutation operator list containing both, pruning and complexifying
  % operators, produces better results than one containing only complexifying
  % parameters.
  mutation_operators := [{atom(), integer()}],
  % Is a list of tuples, where each tuple is composed of a function name, and a
  % function parameter. This allows us to have a population of agents where
  % different agents can use different functions that determine the amount of
  % topological mutations that is used to produce the agent's offspring.
  tot_topological_mutations_fs := [{atom(), float()}],
  % Is an atom specifying the evolutionary loop function, generational or
  % steady_state for example. A population should use only a single evolutionary
  % loop function for all its agents. A particular population_mgr must be fair
  % to all its agents, judging them all in the same way (otherwise, we cannot
  % know whether the agent is superior due to it fitness, or due to its
  % preferential treatment) and so only a single type of evolutionary loop type
  % should be used for a single population. Of course, one could run multiple
  % populations, each using a different type of evolutionary loop, existing in
  % the same scape.
  population_evo_alg_f := generational | steady_state,
  % Is an atom specifying, the name of the selection function. A single
  % population uses a single particular selection function to separate the fit
  % agents from the unfit.
  population_selection_f := atom(),
  specie_distinguishers := [specie_distinguisher()],
  hof_distinguishers := [hof_distinguisher()]
}}.

-type experiment() :: #{data := #{
  % Is the unique Id or name of the experiment being conducted. Because we wish
  % for the DB table to hold numerous experiments, we need to be able to
  % give each experiment its own particular id or name.
  id := atom(),
  % This element is present for the use by the experiment_mgr. When we start the
  % experiment_mgr program with the experiment tuple whose backup_flag is set
  % to false it does not backup that particular experiment to disk. This might
  % be useful when we wish to quickly run an experiment but not write results to
  % the database.
  backup_flag := boolean(),
  % This element will store the PMP record with which the population_mgr was
  % started for this particular experiment. This will allow us to later on
  % know what the experiment was for, and how the population_mgr was started
  % (all the initial parameters) to produce the results and traces in the
  % experiment entry. This way the experiment can be replicated later on.
  pm_parameters := pmp(),
  % Similarly, to the pm_parameters which defines how the population_mgr runs,
  % we also need to remember the parameters of the population itself, and the
  % the experiment to which the traces belong. This information is uniquely
  % identified by the init_constraints list with which the population is
  % created. Having the init_constraints list with which the population is
  % created. Having the init_constraints will allow us to later on replicate
  % experiment if needed.
  init_constraints := [constraint()],
  % This element can be set to two values: << in_progress >> and
  % << completed >>. The experiment is in progress until it has run for
  % << total_runs >>  number of times, and thus the experiment has accumulated
  % total_runs number of traces on its << trace_acc >> list. If, for example,
  % during the experiment run there is a power outage, when we later go through
  % all the experiments in the experiment table, we will be able to determine
  % which of the experiments were interrupted, based on their progress flag.
  % Once the experiment is completed, the progress_flag. is set to:
  % << completed >>.
  progress_flag := atom(),
  % This is a list where we store the trace tuples. If we apply our TXEANN to
  % some particular problem 10 times, and this perform 10 evolutionary runs, we
  % keep pushing new trace tuples into this list until it contains all 10
  % traces, which we can later use at out leisure to build graphs and/or
  % deduce performance statistics.
  trace_acc := [trace()] | [],
  % We plan on running the experiment some << total_runs >> number of times. The
  % run_index keeps track of what the current run index is. If the experiment
  % is interrupted, using this and other parameters we can restart the
  % experiment where we left off.
  run_index := integer(),
  % This element defines the total number of of times that we wish to perform
  % the evolutionary run, the total number of traces to build this particular
  % experiment from.
  total_runs := integer(),
  % This element is the tuple: {date-time, monotonic-time}, which specifies
  % when the experiment was started.
  started := {string(), integer()},
  % Complementary to the << started >> element, this one stores the date() and
  % time() of when the experiment was completed.
  completed := {string(), integer()},
  % Generated every time the experiment has been restarted after an
  % interruption.
  interruptions := [string()]
}}.

%%-----------------------------------------------------------------------------
%% Exoself
%%-----------------------------------------------------------------------------

-type agent() :: #{data := #{
  id := agent_id(),
  encoding_type := encoding_type(),
  % The generation to which this NN system belongs. The seed NN system has a
  % generation of 0. When an offspring is created, its generation is that of
  % its parent +1.
  generation := integer(),
  population_id := population_id() | undefined,
  % The Id of the specie the NN belongs to.
  specie_id := specie_id(),
  % The Id of the cortex of the NN, the cortex element which has the Ids of all
  % the neurons, sensors and actuators of the NN.
  cx_id := cortex_id(),
  % The NN's particular "fingerprint", a tuple composed of the NN's topological
  % structure, and the types of sensors and actuators it is using.
  fingerprint := fingerprint() | undefined,
  % Constraints define the NN's morphological type, what types of sensors,
  % actuators, activation functions, and other features that the NN has access
  % to during its evolution. Different species will have different constraints,
  % and different constraints will define what different elements the NN and its
  % offspring can integrate during evolution. In essence, << constraint >> keeps
  % track of the available parameters, mutation operators, activation functions,
  % morphologies, and various other parameters available to the NN as it evolves.
  % It ensures that the NN using a particular << constraint >> tuple produces
  % offspring related to it in a particular manner, in a manner that ensures
  % that the offspring too can be applied to the same problem, or stay within
  % certain specifications.
  constraint := constraint(),
  % The evolutionary history of the NN, a list of mutation operators that were
  % used on the seed NN system to evolve the current NN based system. It is the
  % NN's evolutionary path from the simple topology it started with, to its
  % current state. The way we can keep track of how the particular NN topology
  % was reached, what path it took, and perhaps extract the why behind it all!
  evo_hist := evo_hist() | [],
  % The NN's current fitness.
  fitness := float(),
  % The number of generations that have passed since the NN last increased in
  % fitness.
  innovation_factor := integer(),
  % The NN's topology.
  pattern := [{float(), [neuron_id()]}] | [],
  % Is an atom specifying the function name. The tuning_selection_f is used to
  % compose a list of tuples: [{NId, Spread},...], where each tuple is composed
  % of a neuron id and the perturbation spread value. The actual tuning function
  % accepts the NIds, the generation value of the agent (its age), the
  % perturbation range value, and the annealing parameter. The selection
  % function then composes the list, and returns that list of tuples to the
  % exoself. Once this list of tuples is composed, the exoself sends each of the
  % selected neurons a message to perturb its synaptic weights using the
  % << Spread >> value. Once the list of selected neurons is composed, we use
  % the IdsNPids ets table which maps ids to pids and back, convert the NIds to
  % NPids, and send each of the selected NPids the noted message.
  tuning_selection_f := atom(),
  % Is an atom specifying the function name. There are numerous ways
  % to implement simulated annealing based on the various properties of the NN
  % system, for example, the neuronal or general agent age. This function
  % should accept as input the list of the selected neuron Ids for
  % perturbation, and then based on it and the perturbation_range parameter,
  % calculate the new and updated perturbation intensity range for each neuron,
  % sending each neuron a signal that it should perturb its weights and the
  % intensity it should use.
  annealing_parameter := float(),
  % Is a tuple composed of an atom specifying the function name, and a
  % parameter: {FunctionName, Parameter}. The max_attempts value could also be
  % computed in numerous ways. It could be a constant, independent of the NN's
  % size. On the other hand, it could be proportional to the size of the NN, the
  % number of neurons selected for perturbation. The input to this function
  % should be the NN size. It must be ensured that all the agents which
  % belong to the same species or population use the same tuning_duration_f. We
  % want the NN based agents that learn the fastest, that are the most dynamic
  % and most general, given all other things are equal, including the amount
  % of time they are given to tune in.
  tuning_duration_f := {atom(), float()},
  % Is a float, a multiplier of math:pi(), which specifies the actual
  % perturbation range. So, for example, if perturbation_range is set to 2,
  % then the actual (before annealing, if any) perturbation range is 2*math:pi()
  % thus the random perturbation value is chosen by running:
  % math:random(2*math:pi()). In this manner, by allowing the constraint record
  % provide a list of perturbation_ranges, as it does with activation functions
  % for example, we can have different agents using different perturbation_range
  % parameters, which will help in experimentation and also make it easier to
  % test this element and its affect on evolution and performance in different
  % problems and applications.
  perturbation_range := float(),
  perturbation_qty := multiple | single,
  % Is a list of tuples [{MO, Probability}] composed of atoms representing the
  % names of mutation operator functions that are available to the agent during
  % its topological mutation phase, and floating point numbers that specify the
  % the proportional probability of that mutation operator being chosen versus
  % another operator in the list. It might be advantageous for different agents,
  % different species, or simply different populations, to have different sets
  % mutation operators. We could even perturb the mutation probabilities, thus
  % making our system have the functionality of any evolutionary strategies
  % based evolutionary computation algorithm.
  mutation_operators := [{atom(), float()}],
  % Is a tuple which specifies the name of the function that produces the total
  % number of topological mutations to be applied to the NN, and a parameter for
  % that function. Currently, our system does this through the function:
  % random:uniform(round(math:pow(TotNeurons, 1/2))). It is a clean approach and
  % works very well, but perhaps we will later wish to try a different method,
  % allow different agents to use different functions, to see which work better
  % in a single population. We could achieve this through this parameter, by
  % letting different agents use such different functions.
  tot_topological_mutations_f := {atom(), float()},
  % The definition of Lamarckian evolution is based on the idea that an organism
  % can pass on to its offspring that characteristics that is has acquired and
  % learned during its lifetime (evaluation), all its knowledge and learned
  % skills. This differs to the more biologically correct Darwinian.
  heredity_type := darwinian | lamarckian,
  substrate_id := substrate_id() | undefined,
  offspring_ids := [agent_id()] | [],
  parent_ids := [agent_id()] | [],
  champion_flag := boolean(),
  fs := float(),
  main_fitness := [float()] | undefined
}}.

-type champion() :: #{data := #{
  hof_fingerprint := [integer()],
  id := agent_id(),
  fitness := [float()],
  validation_fitness := float(),
  main_fitness := float() | undefined,
  tot_n := integer(),
  generation := integer(),
  fs := float()
}}.

%%-----------------------------------------------------------------------------
%% PopulationMgr
%%-----------------------------------------------------------------------------

-type pmp() :: #{data := #{
  op_modes := [op_mode()],
  population_id := atom(),
  % Allows the experiment_mgr to set which percentage of the population
  % survives during the selection phase.
  survival_percentage := integer(),
  % Allows the experiment_mgr to set the size limit of every specie within the
  % the population. This is an important  parameter to define when starting an
  % experiment.
  specie_size_limit := integer(),
  % Allows the experiment_mgr to define the initial size of the specie. For
  % example, the experiment can be started where the initial specie size is set
  % to 1000, but the specie size limit to 100. In this way, there would be a
  % great amount of diversity (given the constraint is defined in such a manner
  % NN based agents have access to a variety of plasticity functions, activation
  % functions,...), but after a while only 100 are allowed to exist at any one
  % time. Or things could be be done in the opposite way, the initial specie
  % size can be small, and the limit specie size large. Allowing the specie to
  % rapidly expand in numbers and diversity, from some small initial bottleneck
  % in the population.
  init_specie_size := integer(),
  % Every experiment needs a termination condition, and the experiment_mgr
  % specifies the generation limit based termination condition for the
  % population_mgr, using this parameter.
  generation_limit := inf | integer(),
  % Lets the experiment_mgr specify the evaluations limit based termination
  % condition.
  evaluations_limit := integer(),
  % Lets the experiment_mgr specify the fitness based termination condition.
  fitness_goal := inf | integer()
}}.

-type stat() :: #{data := #{
  % This is the specie's morphology.
  morphology := atom(),
  % Is the Id of the specie for whom the stat is calculated.
  specie_id := specie_id(),
  % The size of the average NN based agent of the specie, calculated by summing
  % up all the neurons of the specie, and dividing the number by the number of
  % agents belonging to the specie at the time of calculation.
  avg_neurons := float(),
  % The standard deviation of the avg_neurons value.
  std_neurons := float(),
  % Is the average fitness of the agents within the population.
  avg_fitness := [float()],
  % Is the standard deviation of the avg_fitness value.
  std_fitness := [float()] | inf,
  % The maximum fitness achieved by the agents within the specie at the time of
  % calculation.
  max_fitness := [float()],
  % The minimum fitness achieved by the agents within the specie at the time of
  % calculation.
  min_fitness := [float()],
  % The average diversity of the specie.
  avg_diversity := integer(),
  % The number of evaluations  that the agents of this particular specie used
  % during the given X number of evaluations. So, for example, if the population
  % calculates the specie stats every 500 evaluations and there are 2 species in
  % the population, then one specie might have taken 300 of the evaluations if
  % its organisms kept dying rapidly, and the other specie would then have taken
  % the other 200 during that 500 evaluations slot. This value represents the
  % turnover of the specie, and is dependent on the numerous factors, amongst
  % which is of course the number of the species in the population (if only
  % one, then it will take all 500 evaluations), the general fitness of the
  % agents (how often they die or get reevaluated if applied to ALife, for
  % example), and the specie size.
  evaluations := integer(),
  % Finally, we also time stamp the stat record at the moment of calculation.
  time_stamp := integer(),
  % Thus, every X (500 by default) number of evaluations, for every specie we
  % calculate  all the properties of the << stat >>, and thus form a list of
  % N SpecieStat elements, where N is the number of species in the population
  % at the time of calculating the SpecieStats.
  validation_fitness := {float(), agent_id() | void}
}}.

-type trace() :: #{data := #{
  % The SpecieStats list belongs to a list called the population << stats >>.
  % The << stats >> list belongs to the population's trace element. It is called
  % a trace because when we dump the the population's trace to console, we can
  % see the general progress of the population, the number of species, and the
  % the properties of those species, as it is outlined by their stat tuples. It
  % is a trace of the population's evolutionary history, it is the
  % evolutionary path that the population has taken.
  stats := [[stat()]] | [],
  tot_evaluations := integer(),
  % The step_size is defined as the X number of evaluations, such that every X
  % number of evaluations we measure the various evolutionary statistics and
  % properties of the population at hand.
  step_size := integer()
}}.

-type population() :: #{data := #{
  id := population_id(),
  % The list of the specie Ids that belong to this population.
  specie_ids := [specie_id()] | [],
  % List of available morphologies for this population. The list of morphologies
  % defines the list of sensors and actuators available to the NNs in this
  % population. Since the morphology defines the sensors and actuators of the NN
  % system, this list effectively defines the problem or simulation to which
  % the evolving population of NN systems will be applied, and for what purpose
  % agents will be evolved.
  morphologies := [atom()] | [] | undefined,
  % Innovation factor is based on how long ago the average fitness of this
  % problem increased.
  innovation_factor := {integer(), integer()} | undefined,
  % Is an atom, which specifies the name of the evolutionary loop. This could be
  % be << steady_state >> or << generational >>. The population_mgr process
  % should, based on this element, choose the evolutionary loop function which
  % will then, using the selection_f, deal with the population of agents,
  % select them, mutate them, keep track of the population's progress...
  evo_alg_f := atom(),
  % Is an atom, which specifies the name of the selection function. This
  % function accepts the list of agent Ids and their true fitness, and produces
  % a list of fit agents, and the number of offspring each is allotted. When
  % executed within the steady-state evolutionary loop, this value
  % (alloted number of offspring) is converted into a percentage of the agent
  % being selected for the creation of a single offspring to replace it, and a
  % percentage that the agent itself will be released back into the simulated
  % world for reevaluation instead of producing an offspring.
  selection_f := atom(),
  trace := trace() | undefined
}}.

-type specie() :: #{data := #{
  id := specie_id(),
  % The Id of the population that this specie belongs to.
  population_id := population_id(),
  % The particular rough identification of the species, any NN with the same
  % fingerprint belongs to this species.
  fingerprint := fingerprint(),
  % Constraint specifies the list of sensors, actuators, activation functions,
  % etc. that the agents belonging to this species have access to. And the name
  % of the morphology of this species.
  constraint := constraint(),
  fitness := {[float()], [float()], [float()], [float()]},
  % Innovation factor is based on how long ago the average fitness of this
  % specie increased.
  innovation_factor := {integer(), integer()},
  stats := [[stat()]] | [],
  seed_agent_ids := [agent_id()] | [],
  hof_distinguishers := [atom()],
  specie_distinguishers := [atom()],
  hall_of_fame := [champion()] | []
}}.

-type population_status() :: #{data := #{
  op_tag := pause | continue | done,
  pop_gen := integer(),
  eval_acc := integer(),
  cycle_acc := integer(),
  time_acc := integer(),
  tot_evaluations := integer(),
  goal_reached := boolean()
}}.

%%=============================================================================
%% Model functions
%%=============================================================================

%%-----------------------------------------------------------------------------
%% @doc topology_summary data object.
%% @end
%%-----------------------------------------------------------------------------
-spec topology_summary(map()) -> topology_summary().
topology_summary(TopologySummary) ->
  #{data => TopologySummary}.

%%-----------------------------------------------------------------------------
%% @doc sensor data object.
%% @end
%%-----------------------------------------------------------------------------
-spec sensor(map()) -> sensor().
sensor(Sensor) ->
  #{data => Sensor}.

%%-----------------------------------------------------------------------------
%% @doc actuator data object.
%% @end
%%-----------------------------------------------------------------------------
-spec actuator(map()) -> actuator().
actuator(Actuator) ->
  #{data => Actuator}.

%%-----------------------------------------------------------------------------
%% @doc neuron data object.
%% @end
%%-----------------------------------------------------------------------------
-spec neuron(map()) -> neuron().
neuron(Neuron) ->
  #{data => Neuron}.

%%-----------------------------------------------------------------------------
%% @doc cortex data object.
%% @end
%%-----------------------------------------------------------------------------
-spec cortex(map()) -> cortex().
cortex(Cortex) ->
  #{data => Cortex}.

%%-----------------------------------------------------------------------------
%% @doc substrate data object.
%% @end
%%-----------------------------------------------------------------------------
-spec substrate(map()) -> substrate().
substrate(Substrate) ->
  #{data => Substrate}.

%%-----------------------------------------------------------------------------
%% @doc constraint data object.
%% @end
%%-----------------------------------------------------------------------------
-spec constraint(map()) -> constraint().
constraint(Constraint) ->
  #{data => Constraint}.

%%-----------------------------------------------------------------------------
%% @doc experiment data object.
%% @end
%%-----------------------------------------------------------------------------
-spec experiment(map())-> experiment().
experiment(Experiment) ->
  #{data => Experiment}.

%%-----------------------------------------------------------------------------
%% @doc agent data object.
%% @end
%%-----------------------------------------------------------------------------
-spec agent(map()) -> agent().
agent(Agent) ->
  #{data => Agent}.

%%-----------------------------------------------------------------------------
%% @doc champion data object.
%% @end
%%-----------------------------------------------------------------------------
-spec champion(map()) -> champion().
champion(Champion) ->
  #{data => Champion}.

%%-----------------------------------------------------------------------------
%% @doc pmp data object.
%% @end
%%-----------------------------------------------------------------------------
-spec pmp(map()) -> pmp().
pmp(PMP) ->
  #{data => PMP}.

%%-----------------------------------------------------------------------------
%% @doc stat data object.
%% @end
%%-----------------------------------------------------------------------------
-spec stat(map()) -> stat().
stat(STAT) ->
  #{data => STAT}.

%%-----------------------------------------------------------------------------
%% @doc trace data object.
%% @end
%%-----------------------------------------------------------------------------
-spec trace(map()) -> trace().
trace(Trace) ->
  #{data => Trace}.

%%-----------------------------------------------------------------------------
%% @doc population data object.
%% @end
%%-----------------------------------------------------------------------------
-spec population(map()) -> population().
population(Population) ->
  #{data => Population}.

%%-----------------------------------------------------------------------------
%% @doc specie data object.
%% @end
%%-----------------------------------------------------------------------------
-spec specie(map()) -> specie().
specie(Specie) ->
  #{data => Specie}.

%%-----------------------------------------------------------------------------
%% @doc population_status data object.
%% @end
%%-----------------------------------------------------------------------------
-spec population_status(map()) -> population_status().
population_status(PopulationStatus) ->
  #{data => PopulationStatus}.

%%=============================================================================
%% API
%%=============================================================================

%%-----------------------------------------------------------------------------
%% @doc The get function fetches the value for the given key in the model.
%% @end
%%-----------------------------------------------------------------------------
-spec get([key()] | key(), model()) -> [not_found | value()] | not_found | value().
get(Keys, #{data := Data}) when is_list(Keys) ->
  [maps:get(K, Data, not_found) || K <- Keys];
get(K, #{data := Data}) ->
  maps:get(K, Data, not_found).

%%-----------------------------------------------------------------------------
%% @doc The set/2 function updates the value in the model for the key/value
%%      pair or for each key/value pair in the list.
%% @end
%%-----------------------------------------------------------------------------
-spec set([{key(), value()}] | {K :: key(), V :: value()}, Model :: model()) -> Model :: model().
set([H | Tail], Model) ->
  NewModel = set(H, Model),
  set(Tail, NewModel);
set([], Model) ->
  Model;
set({K, V}, #{data := Data} = Model) ->
  NewModel = Model#{data := Data#{K => V}},
  NewModel.