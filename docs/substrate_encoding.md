### Step-by-step functionality of the substrate process
--------------------

(see substrate_encoding screen shot)

1. Exoself spawns neurons, sensors, actuators, substrate_cpps, substrate_ceps,
substrate, and the cortex process.
2. Cortex sends the sync message to all the sensors, calling them to action.
3. Sensors poll the environment for sensory signals.
4. Sensors do postprocessing of the signals.
5. Sensors forward the processed sensory signals to the substrate.
6. Substrate process gathers all the signals from the sensors, and based on those
signals, its densities, and the actuators, constructs a substrate if its substrate_state_flag
is set to reset. If substrate_state_flag is set to hold, go to next step.
7. Substrate sends the coordinates of the connected neurodes to the substrate_cpps
it is connected to.
8. The cpps process the coordinates
9. The cpps forward the processed coordinate vectors to the neurons they are connected
to in the NN.
10. NN processes the coordinate signals.
11. The neurons in the output layer of the NN produce output signals, which are
then sent to the ceps they are connected to.
12. The ceps wait and gather the signals from all the neurons with whom they have
presynaptic links. The ceps process the accumulated signals.
13. The ceps forward the vector signals to the substrate.
14. The substrate process calls the cpps for every connected neurode in the substrate.
Once all the neurodes have their synaptic weights, the substrate maps the
signals from the sensors to the input hyperlayer. It then processes the sensory
signals, until at some later point the output hyperlayer contains the output signals.
15. Each hyperplane in the output hyperlayer is associated with its own actuator, to
which the output vector is then forwarded to.
16. Actuators gather the signals sent to them from their fanin_ids list (in this case
the id of the substrate process).
17. Actuators use the signals to take action and interact with the environment they
are interfacing with.
18. Actuators send the sync message back to the cortex.
19. The cortex gathers all the sync messages from all its actuators.
20. The cortex calls sensors to action, for another Sense-Think-Act loop. Go to
step 3. 

#### Sources
--------------------
G.I. Sher, Handbook of Neuroevolution Through Erlang. Springer 2012, print ISBN: 978-1-4614-4462-6 ebook ISBN: 978-1-4614-4463-6.