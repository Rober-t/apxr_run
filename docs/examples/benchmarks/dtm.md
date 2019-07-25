## Discrete T-Maze Simulation


### Introduction
--------------------

The T-Maze problem is a standard problem that is used to test the ability
of a NN based system to learn and change its strategy while existing in, and
interacting with, a maze environment. At one horizontal end of the maze is a low
reward, and at another a high reward. The agent is a simulated robot which
navigates the maze. Every time the robot crashes into a wall or reaches one of
the maze’s ends, its position is reset to the start of the maze. The whole
simulation run (agent is allowed to navigate the maze until it either finds the
reward and its position resets to base, or crashes into a wall and its position
is reset to base) lasts X number of maze runs, which is usually set to 100. At
some random time during those 100 maze runs, the high and low reward positions
are swapped. The goal is for the agent to gather as many reward points as
possible. Thus, if the agent has been reaching the high reward end of the maze,
and suddenly there was a switch, the best strategy is for the agent when it has
reached the location of where previously there was a high reward, is to realize
that it now needs to change its strategy and always go to the other side of the
maze, for the remainder of the simulation. To do this, the agent must remember
what reward it has picked up and on what side, and change its traveling path
after noticing that the rewards have been switched, which is most easily done
when some of the agent’s neurons are plastic. 

We use a simplified version of the T-Maze problem. It is used widely and it does
not require an entire 2D environment and robot simulation to be simulated.

The agents traveling through the maze will be able to move forward, and turn
left or right, but there will be no width to the corridors. The corridors will
have a certain discrete length, and the agent will see forward in a sense that
its range sensor will measure the distance to the wall ahead, and its side
sensors will measure a distance to the sides of the "corridor" it is in, which
when traveling down a single dimensional corridor will be 0, yet when reaching
the T intersection, will show that it can turn left or right. The turns
themselves will be discrete 90 degree turns, thus allowing the agent to turn
left or right, and continue forward to gather the reward at the end of the
corridor. This version of the T-Maze though simple, still requires the agent to
solve the same problem as the non discrete Maze. In the discrete version, the
agent must still remember where the reward is, evolve an ability to move down
the corridors and turn and move in the turned direction where there is space to
move forward, and finally, remember on which side of the maze it last found the
highest reward.

The T-Maze will be contained in a private scape, and the movement and senses
will, as in the previous simulation, be done through the sending and receiving
of messages. Because we will create a discrete version of the maze, we can
simulate the whole maze by simply deciding on the discrete length of each
section of the corridor, and what the agent will receive as its sensory signals
when in a particular section of the maze. The agent will use a combination of
the following two sensors:

- distance_sensor: A laser distance sensor pointing forward, to the left side,
and to the right side, with respect to the simulated robot’s direction. Since
the maze is self contained and closed, the sensors will always return a
distance. When traveling down the single dimensional corridor, the forward
sensor will return the distance to the wall ahead, and the side distance sensors
will return 0, since there is no place to move sideways. When the agent reaches
an intersection, the side range sensors will return the distances to the walls
on the side, thus the agent can decide which way to turn. If the agent has
reached a dead end, then both the forward facing, and the side facing range
sensors will return 0, which will require the agent to turn, at which point it
can start traveling in the other direction.
- reward_consumed: The agent needs to know not only where the reward is, but
how large it is, since the agent must explore the two rewards, and then for the
remainder of the evaluation go towards the larger reward. To do this, the agent
must have a sensory signal which tells it how large the reward it just consumed
is. This sensor forwards to the NN a vector of length one: [RewardMagnitude],
where RewardMagnitude is the magnitude of the actual reward.

The agent must also be able to move around this simplified, discrete labyrinth.
There are different ways that we could allow the NN based agent to control the
simulated robot within the maze. Due to this being a discrete version of the
maze, it can easily be represented as a state machine, or simply as a list of
discrete sections. Due to this being a discrete version of the maze, it can
easily be represented as a state machine, or simply as a list of discrete
sections. The agent starts at the bottom of the T-Maze located
at {X = 0, Y = 0}, it can then move up to {0, 1}, which is an intersection.
At this point the agent can turn left and move a step forward to {-1, 1}, or
turn right and move a step forward to {1, 1}. 

We will let each discrete sector keep track of the following:
  - id: It’s own id, its Cartesian coordinate.
  - r: The reward the agent gets for being in that sector. There will be only
    two sectors that give reward, the two horizontal endings of the "T". This
    reward will be sensed by the reward_sensor.
  - description: This will be the list that contains all the sensory information
    available when the agent is in that particular sector. In this simulation it
    will contain the range sensory signals.

These sectors will all be contained in a single record’s list, used by the
private scape which represents the entire maze.


### Steps
--------------------

The scape will keep track of the agent's position and orientation, and be able
to act on the messages sent from its sensor and actuator, and based on them
control the agent’s avatar. 

The T-Maze will start with the large and small rewards at the two opposite
sides of the T-Maze, and then at some random maze run to which the switch_event
is set (different for each evaluation), the large and small reward locations
will flip, and require for the agent to figure this out and go to the new
location if it wants to continue collecting the larger of the two rewards.

As per the standard T-Maze implementation, the large reward is worth 1 point,
and the small reward is worth 0.2 points. If at any time the agent hits a wall,
by for example turn/moving when located at the base of the maze, and thus
hitting the wall, the maze run ends and the agent is penalized with -0.4 fitness
points, is then re-spawned at the base of the maze, and the run_index is
incremented. If the agent collects the reward, the maze run ends and the agent
is re-spawned at the base of the maze, with the run_index incremented.

Finally, once the agent has finished total_runs number of maze runs, the
evaluation of the agent’s fitness ends, at which point the exoself might
perturb the NN’s synaptic weights, or end the tuning run... To ensure that the
agents do not end up with negative fitness scores when setting the total_runs to
100, we will start the agents off with 50 fitness points. Thus an agent that
always crashes will have a minimum fitness score of 50 - 100 * 0.4 = 10.


### How to run
--------------------

    $ rebar3 shell
    1> experiment_mgr:run().


### Expected results
--------------------

An evaluation is composed of 100 total maze runs, and sometime during the
midpoint, between run 35 and 65, the high and low rewards are flipped. 

In this implementation, we set the switch_event to occur on the run number:
35 + rand:uniform(30). It will take at least one wrong trip to the reward to
figure out that its position has been changed. Also, we should expect that
eventually, evolution will create NNs that always first go to the maze corner
located at [1, 1], which holds the high reward before it is flipped. 

So then, the maximum possible score achievable in this problem, a score
representing that the problem has been solved, is: 99 * 1 + 1 * 0.2 + 50 =
149.2, which represents an agent that first always goes to the right corner, at
some point it goes there and notices that the reward is now small (0.2 instead
of 1), and thus starts going to the [-1, 1] corner. This allows the agent to
achieve 99 high rewards, and 1 low reward. A score which represents that the
agent evolved to always go to {1, 1}, is at most: 65 * 1 + 35 * 0.2 + 50 = 122,
which is achieved during the best case scenario, when the reward is flipped on
the 65th count, thus allowing the agent to gather high reward for 65 maze runs,
and low reward for the remaining 35 maze runs. 

The agent will perform multiple evaluations, during some evaluations the
reward switch event will occur early, and every once in a while it will occur
on the 65th maze run, which is the latest time possible. During that lucky
evaluation, the agent can reach 122 fitness points by simply not crashing and
always going to the {1, 1} side. The agent can accomplish this by first having:
0.33 > Output >- 0.33, which will make the avatar move forward, and during the
second step have Output > 0.33, which will make the avatar turn right and move
forward to get the reward.

Finally, the smallest possible fitness is achieved when the agent always
crashes into the wall: 50 - 100 * 0.4 = 10.

When plasticity is enabled a number of the trace fitness scores should to be >
122. When plasticity is not enabled, fitness scores should max out at 122.


### Related publications
--------------------

[Handbook of Neuroevolution Through Erlang](http://www.amazon.com/Handbook-Neuroevolution-Through-Erlang-Gene/dp/1461444624) _by Gene Sher_.