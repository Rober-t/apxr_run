## FX (Evolving Currency Trading Agents)


### Introduction
--------------------

The application of Neural Networks to financial analysis in general,
and currency trading in particular, has been explored for a number of years.
In this application we create a Forex simulator, and then use our
neuroevolutionary system to evolve automated currency trading agents.

For this application we will utilize not only the standard sliding window
approach when feeding the sensory signals to the neural encoded agents,
but also the sliding chart window, where we feed evolved substrate encoded
agents the actual candle-stick price charts, and then compare the performance
of the two approaches. 

We will compare the SENN based traders which use Price Chart Input (PCI),
to the standard, direct encoded NN based trading agents which use Price List
Input (PLI), in which the time series of closing prices is encoded as a list of
said prices and/or other technical indicators.

Our goal is to implement and test the utility of using graphical input of the
time series, the use of candle-stick style chart as direct input to the
geometry sensitive NN systems evolved using our TWEANN, and then to benchmark
the performance.

We will develop agents to predict the future price of a financial instrument,
and then based on the prediction have the agent trade the financial instrument
autonomously, without any humans in the loop.

The needed new features to make our TWEANN work within this field can also be
used in time series analysis problems not related to finance.


### Substrate encoding
--------------------

Substrate encoding has a natural property of taking geometrical properties of
the sensory signals into consideration, and it can through its own geometrical
topological structures, further extract and reveal the geometrical regularities
within the data.

With this type of indirect encoded neural networks we can analyze the price
charts directly, making use of the geometrical patterns, and trends within.
Because each neurode in the substrate receives a connection from every neurode
or input element in the preceding hyperlayer, the chart that is fed to the
substrate must first be reconstructed to the resolution that still retains the
important geometrical information, and yet is computationally viable as input.

For example, if the sliding chart that is fed to the substrate is 1000x1000,
which represents 1000 historical points (horizontal axis), with the resolution
of the price data being (MaxPlotPrice – MinPlotPrice)/1000 (the vertical axis),
then each neurode in the first hidden processing hyperlayer of the substrate
will have 1000000 inputs. If the substrate has three dimensions, and we set it
up such that the input signals are plane encoded and located at Z = -1, with a
10X10 neurodes in the hidden hyperplane located at Z = 0, and 1X1 neurodes in
the third hyperplane located at Z=1 then each of the 100 neurodes at Z = 0
receives 1000000 inputs, so each has 1000000 synaptic weights, and for this
feedforward substrate to process a single input signal would require
100*1000000 + 1*100 calculations, where the 1*100 calculations are performed by
the neurode at Z = 1, which is the output neurode of the substrate. This means
that there would be roughly 100000000 calculations per single input,
per processing of a single frame of the price chart.

Thus it is important to determine and test what resolution provides enough
geometrical detail to allow for prediction to be made, yet not overwhelm the
NN itself and the processing power available to the researcher. Once the number
of the historical prices (horizontal axis on the price chart) and the
resolution of the prices (vertical axis on the price chart) are agreed upon,
the chart can then be generated for the sliding window of the currency pair
exchange rates, producing the sliding chart. 


### Forex simulator
--------------------

Each interfacing NN will be given a $300 starting balance. Each agent will
interface with its own private scape for which it will produce an output which
will be converted by its actuator to – 1 if it’s less than -0.5, 0 if
between -0.5 and 0.5, and 1 if greater than 0.5.

When interacting with the Forex simulator, -1 means go short, 0 means close
position (or do nothing if no position is open), and 1 means go long
(if you currently have a short position opened, then first close the position,
and then go long). The Forex simulator will simulate the market using real
EUR/USD currency pair closing prices using 15 min ticks. The simulator will use
a price spread of $0.00015, which is about average for a standard account from
a financial service providers.

Because we will want to test the generalization of our evolved agents, their
ability to be applied to previously unseen financial price data and successfully
use it to make trades, we take the dataset and further split it into training
and generalization subsets. 

Finally, when the agent is opening a position, it is always done with $100
leveraged by x50 to $5000. Thus the losses and gains are based on the $5000
opened order. The leverage of x50 is a standard one provided by most brokers,
since the change in currency pair prices is very low, profit is made by trading
high volumes of the same. 


### Details
--------------------

1. The sensors of the agent poll the scape for sensory data.
3. The scape looks inside its database for the currency exchange rates, and
   based on the resolution/length of the historical currency exchange rate,
   builds a price list of that resolution.
4. The calling function is returned the price list.
5. Based on the receive clause, whether it is PCI or PLI, it encodes the
   returned price list accordingly, either as a simple price list, or as a
   price chart using trinary (-1,0,1) encoding.
6. The sensory signal (PCI or PLI, and the Internals) are forwarded to the
   agent’s sensors.
7. The agent processes the sensory signals.
8. Based on the sensory signals and the agent’s reasoning about them, the agent
   produces an output, and with its actuator forwards it to the scape to make a
   trade.
9. The receive clause forwards the trading account made by the agent to the order
   handling function of scape.
10. The signal is forwarded to the account processing function.
11. The scape accesses the agent’s account.
12. The queries the database for the current currency pair exchange rate.
13. The database checks the current currency pair exchange rate.
14. The database returns the current currency pair exchange rate, but at the
    same time moves the index of the “current” time step, to the next time step,
    advancing one tick forward in the simulated market.
15. The scape executes the agent’s order. But also, knowing the exchange rate
    of the next tick, calculates the profit/loss/change within the agent’s net
    worth.
16. Based on whether the simulation has ended, which occurs when the index used
    in the exchange rate database has reached ‘$end_of_table’, or the agent’s
    net worth has dipped below $100, the function returns to the calling
    receive clause a response message to be sent back to the actuator.
17. The scape returns back to the actuator the tuple: {Fitness, HaltFlag},
    where the Fitness is set to 0 when the HaltFlag is set to 0, and it is set
    to the agent’s net worth when the HaltFlag is set to 1 (when a termination
    condition has been reached).
18. At this point the loop repeats and we go to step 1 if termination condition
    has not been reached. 


### How to run
--------------------

    $ rebar3 shell
    1> experiment_mgr:run().


### Experiment
--------------------

A single evaluation of a NN is counted when the NN based agent has went
through all the training data points, or if its balance dips below $100.
The fitness of the NN is its net worth at the end of its evaluation. Each
evolutionary run lasts for 25000 evaluations, and each experiment is
composed of 10 such evolutionary runs. In each experiment the population size
is set to 10. Finally, in every experiment we allow the NNs to use and
integrate through evolution the following set of activation functions:
[tanh, gaussian, sin, absolute, sgn, linear, log, sqrt].

We set the NNs to use price sliding window vectors for direct encoded NNs,
and price charts for substrate encoded NNs. We also connect each agent not
only to the sensors providing them with closing prices, but also the fx_sensor
which produces the vector composed of: [Position, Entry, PercentageChange],
where Position takes the value of either -1 (currently shorting the held order),
0 (no position), or 1 (currently going long on the held order), Entry is the
price at which the position was entered (or set to 0 if no position is held),
and PercentageChange is the percentage change in the position since entry, and
finally the substrate’s own output, a vector of length 1, will be fed back to
the substrate’s input hyperlayer. This will, due to feeding the substrate its
own output, make the substrate Jordan Recurrent.

Each experiment is composed of 10 evolutionary runs from which the experiment’s
average/max/min is calculated for both the training and the generalization
testing. Through the experiments we will compare the performance of PCI based
NNs and the PLI based NNs. The sliding window and chart resolution is
comparable for both the neural and substrate encoded NN based agents. 

We set the benchmarker to test generalization abilities of the evolved NN
based agents every 500 evaluations, applying the best NN in the population at
that time to the generalization test. Performing the generalization tests
consistently throughout the evolution of the population will not only allow us
to test the generalization ability of the best NNs in the population, but it
will also allow us to build a plot of the general generalization capabilities
of that particular encoding and sensor type, and the generalization abilities
of our TWEANN in general. Doing this will allow us to get a better idea of
whether generalization drops off as the PCI and PLI NNs are trained,
whether it improves, or whether it stays the same throughout the training
process. 

### Related publications
--------------------

[Handbook of Neuroevolution Through Erlang](http://www.amazon.com/Handbook-Neuroevolution-Through-Erlang-Gene/dp/1461444624) _by Gene Sher_.