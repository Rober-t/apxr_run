## Flatland (Predator vs Prey Simulation)


### Introduction
--------------------

Our simulation is composed of a two dimensional environment, with food
scattered throughout some region, where that food is represented as green
circles. The environment is populated by simulated 2D herbivore robots,
simulated as blue circles, and 2D predator type simulated robots, represented
by red circles with small spears at one end.

Unlike the benchmark scapes, flatland is a public scape. The agents when created
will not spawn this scape, but join/enter it.

The simulated robots are, in essence, avatars controlled by the NNs. 


### Details
--------------------

Flatland creates a simulated representation, an avatar, for the NN that
enters it. When a NN is spawned, it first sends the flatland a message
that it wishes to enter the scape. At this point the scape checks the NN
morphology (specified by the message the exoself sent it), and based on that
morphology creates an avatar for it.

The scape also registers the NN as currently possessing an avatar within the
scape, such that when the NN starts sending it action messages to control the
avatar, and polls it for sensory messages based on the sensors of its avatar,
the scape can move its avatar, and forward to it the sensory signals from its
avatar, respectively.

For the simulation to be consistent, and for the world to have a constant or
growing population, we have to use the steady_state based evolution,
rather than generational. The steady_state evolution reflects the continuous
birth and death of agents within an environment, unlike generational evolution
in which the entire generation must die before new agents are created, and a new
generation is spawned.


### Synchronization
--------------------

With regards to the schedule of when to perform collision detection,
we let the scape perform collision detection and other types of environment
calculations after receiving every single action message from an agent.

This has an interesting advantage in that the smaller NNs will
have faster “reflexes”, because the smaller NNs will have lower NN topology
depth, and thus go through more sense-think-act cycles for any given time than
those NNs that are much deeper.

This is somewhat similar to the real world, and the response times
between various organisms, based on their brain structure and its complexity. 


### How to run
--------------------

    $ rebar3 shell
    1> experiment_mgr:run().


### Expected results
--------------------

In this simulation the prey agents learn how to navigate the 2D world, and eat
the plants while avoiding the predators. At the same time, the predators learn to
navigate the 2D world, and hunt the prey. Thus the two species co-evolve, learn
how to evade and hunt, improve their strategies over time, and learn some very
clever trapping and baiting methods (in the case of the predators).

Because the evolutionary paths of the two species are so dependent on which
of the species learn to navigate, evade, and hunt first, the averages of the
evolutionary runs are meaningless. This is due to the fact that when the prey
learn to navigate through the flatland and eat the plants before the predators
learn how to hunt them efficiently, the prey are able to achieve high fitness scores,
while the predators do not do as well. On the other hand, during evolutionary
runs in which the predator specie are able to evolve agents which can navigate
and hunt, before the prey evolve agents which can evade the predators, the
predators achieve high fitness, and the prey do not do as well.

Therefore, instead of using averages, you should chose to plot the results
of a single such evolutionary run.

Because the flatland is be populated by a population of size 10 of prey,
and a population of size 10 of predators, we are be able to see in the plots the
interaction and correlations between the two competing species.

The grpahs incl:

- Average Fitness Vs. Evaluations

  The average fitness for the prey drops dramatically with predators around. Almost
  during every simulation, eventually the predators learn to navigate effectively,
  and attack the prey that passed nearby. 

- NN Size Vs. Evaluations

  The predators, though possessing complex behaviors, ended up with much smaller
  neural networks than the prey. This is probably because of the high turnover
  of the prey, and their need to deal with moving and dynamic predators.

- Population Diversity Vs. Evaluations

  Almost every agent tends to be different from every other agent in each species.

- Population Turnover (death rate) Vs. Evaluations

  Though both species start off equally, the predators quickly learn to hunt the
  prey, and thus the prey’s turnover increases while that of the predator’s
  decreases. Typically, after 30000 (you must set the default limit higher)
  evaluations the turnover for both species reaches an equilibrium. The
  predators maintain a turnover of about 150 evaluations per 500 evaluations.
  Complementary, the prey stay within a turnover average of 350 evaluations per
  every 500 evaluations. 


Because we calculate the specie statistics every 500 evaluations, the
Turnover Vs. Evaluations shows the death rate of the particular specie with
relation to another for those 500 evaluations.

Thus for example if both species survive for an equal amount of time, both
will have a turnover of 250. On the other hand if it’s an open hunting season on
prey, and the predators are just running around eating the prey, the prey will have
a very high turnover, while the predators will live for a much longer number of
cycles, and thus have a much lower turnover. 


### Related publications
--------------------

[Handbook of Neuroevolution Through Erlang](http://www.amazon.com/Handbook-Neuroevolution-Through-Erlang-Gene/dp/1461444624) _by Gene Sher_.