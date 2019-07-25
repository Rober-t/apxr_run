## Double Pole Balancing Simulation


### Introduction
--------------------

In this problem we try to balance two poles of differing lengths at the same
time. The closer the lengths of the two poles are, the more difficult the
problem becomes. The length of one pole is set to 0.1 meters, and the
length of the second is set to 1.

Defined more specifically, the pole balancing problem is posed as follows:

Given a 2D simulation of a cart on a 4.8 meter track, with two poles of
length L on the top of a cart, attached to the cart by a hinge, and thus free to
swing, the NN based controller must apply a force, pushing it back and forth on
the track, such that the poles stay balanced on the cart and within 36 degrees
of the cart's vertical. For sensory inputs, the NN based agent is provided with
the cart's position and velocity. The output of the NN based agent is the force
value F in newtons (N), saturated at 10N of magnitude. Positive F pushes the
cart to the left, and negative pushes it to the right. Given these conditions,
the problem is to balance the poles on the cart for 30 minutes, or as long as
possible, where the fitness is the amount of time the NN can keep the poles
balanced by pushing the cart back and forth.

The temporal granularity of the simulation is 0.01 seconds, which means that
every 0.01 seconds we perform all the physics based calculations, to determine
the position of the cart and the poles. The Agent requests sensory signals and
acts every 0.02 seconds. The simulation termination conditions are as follows:
the cart must stay on the 4.8 meter track or the simulation ends, the simulation
also ends if either of the poles fall outside the 36 degrees of the vertical.

It is possible to very rapidly move the cart back and forth, which keeps the
poles balanced. To prevent this type of solution, the problem is further
modified with the fitness of the NN based agent not only being dependent on the
amount of time it has balanced the pole, but on how smoothly it has pushed the
cart. One fitness function simply rewards the NN based on the length of time it
has balanced the pole, while the other rewards the NN based on the length of
time it has balanced the pole, and penalizes it for very high velocities and
rapid velocity changes. The former is the standard fitness function while the
later is called the damping fitness function.

There are two versions of this problem:

- The sensory signal gathered by the NN is composed of the cart's position and
velocity (CPos, CVel), the first pole's angle and velocity (P1Angle, P2Vel), and
the second pole's angle and velocity (P2Angle, P2Vel).
- The second more complex version of the problem only provides the NN with
partial state information, the cart's position, and the first and second pole's
angular position. This requires the NN based agent to derive the velocities on
its own, which can be done by evolving a recurrent NN topology.

Each double pole balancing simulation is contained and wrapped in a private
scape, represented as a single process.


### Steps
--------------------

The simulation is composed of the following steps:

1. PB (pole balancing) private scape is spawned. 
2. The PB scape initializes the physical simulation, with the first pole’s
initial angle from the vertical randomly selected to be between -3.6 and 3.6
degrees, and the second pole’s angle set to 0 degrees. Furthermore, the first
pole’s length will be set to 1 meter, and 0.1 meter for the second one.
3. The PB process drops into its main loop, and awaits for sense and push
messages.
4. DO:
    5. If {FromPid, sense, Parameters} message is received: The Parameters value
    specifies what type of sensory information should be returned to the caller.
    If the Parameters value is set to 3, then the scape will return the cart,
    pole_1, and pole_2 positions. If Parameters is set to 6, then the scape will
    return the cart position and velocity, and the pole_1 and pole_2 angular
    positions and velocities.
    6. If {FromPid, push, Force, Parameters} message is received: The PB scape
    applies the force specified in the message to the cart, and calculates the
    results of the physical simulation. The response to the push are calculated
    for two 0.01s time steps, taking the simulation 0.02 seconds forward, and
    then returning the scape back to waiting for the sense/push messages again.
    Furthermore, the Parameters value will have the form: << DampingFlag >>,
    where the DampingFlag parameter specifies whether the fitness function
    will be calculated with damping features to prevent the rapid shaking of
    the cart.

UNTIL: Termination condition is reached (goal number of time steps, or one of
the boundary condition breaches).


### Termination condition
--------------------

The termination condition is considered to be any one of the following:
  - The simulation has run for 30 simulated minutes, which is composed of 90000
0.02 second time steps.
  - The pole has deviated 36 or more degrees from the cart’s vertical.
  - The cart has left the track. The track itself is 4.8 meters long, and the cart
will start at the center, and thus be 2.4 meters away from either side. If it
goes beyond -2.4 or 2.4 point on the axis of the track, the termination
condition is reached.


### How to run
--------------------

    $ rebar3 shell
    1> experiment_mgr:run().


### Expected results
--------------------

#### Double pole without damping

    ∼ 2,300 evaluations

#### Double pole with damping

    ∼ 2,300 evaluations


### Related publications
--------------------

[Handbook of Neuroevolution Through Erlang](http://www.amazon.com/Handbook-Neuroevolution-Through-Erlang-Gene/dp/1461444624) _by Gene Sher_.