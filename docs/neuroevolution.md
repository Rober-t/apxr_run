## Notes from [Handbook of Neuroevolution Through Erlang](http://www.amazon.com/Handbook-Neuroevolution-Through-Erlang-Gene/dp/1461444624) _by Gene Sher_.


### Introduction
--------------------

Today’s most advanced approaches to computational intelligence are through neuroevolution, a combination of artificial neural networks and evolutionary computation. 

Neuroevolution is based on the extrapolation of the concepts we’ve learned in neuroscience and evolutionary biology, and the application of those concepts to machine intelligence. 

Both, evolution and neurocomputation, are highly distributed and concurrent problem solving approaches. Evolution is the process of change in the inherited traits within populations due to the constant competition of the organisms within it. 

Neuroevolution problem domains:
- Optimization: You have a problem to solve, you know what you want but you don’t exactly know how to achieve that result, thus you need an intelligent agent to figure out the patterns of this problem for you and come up with an efficient solution.
- Neurocontroller: You have a complex task that needs to be performed, the task itself changes and thus you need an intelligent agent to learn how to perform this task through experience, learn how to use the tools necessary to perform the task, and perform that task efficiently and without tiring. 
- Invention: You have a rough idea or a database of already existing inventions and you want to create a system that will improve on the designs, creating new patents. Or you wish to explore new designs in previously unexplored fields. For this you need an intelligent agent that can extrapolate ideas from existing designs, or come up with and explore new ideas within various fields. 

With regards to the why of neuroevolution, the answer stands with the fact that neuroevolutionary systems, particularly the Topology and Weight Evolving Artificial Neural Networks (TWEANNS), are the most advanced forms of computational intelligence creation. 

TWEANN systems use evolutionary processes to evolve complex systems with neural elements acting as the main building blocks. 

When neural network based intelligent agents are mentioned, we are simply referring to programs that use NNs to process their sensory signals and use their actuators when interacting with the world.

The reason that we need to make our simulations of environments and Xs as detailed as possible is because real sensors, actuators, environments, motors... are flawed, and there is always noise in the data which needs to be taken into account when evolving intelligent agents, so that they are ready for this noise when uploaded to real bodies. The more detailed the simulations, the greater the chance that a NN evolved to control a simulated X, will be able to just as effectively control a real X. 

The goal of ALife is to study logic and emergent phenomena of living systems in simulated environments. Neuroevolution allows us to populate these simulated worlds with learning organisms. Through neuroevolution the simulated environments allow the behaviour of these artificial organisms to evolve over time, changing as the organisms interact with the environment and compete against each other. 

If the software based sensors and actuators are themselves implemented as nodes, similar to how neurons are implemented, then through mutation operators they too can be added and removed to and from the NN during evolution. Through such an implementation we can then allow a neuroevolutionary system to evolve not merely the brain of the artificial organism, but also its morphology. Using this approach, the evolutionary processes will allow for the created mutant offspring to expand and try out different combinations of sensors and actuators, and thus potentially different types of bodily morphologies. 

Pattern of applying a neuroevolutionary system to a problem:
- Create a virtual environment (scape) for the problem, where if the goal is to simply train the NN on some training set (rather than interact with some simulation), then that virtual environment should interface with the NN’s sensors and actuators, present to it the training set, and gage the NN’s performance. 
- Let all the NNs in the population solve the problem, or be trained on some training set until the terminating condition is reached, at which point the scape scores each NN’s performance. 
- Sort the NNs in the population based on their performance.
- Choose some percentage of the top performing NNs, and use their genotypes to generate mutant offspring by mutating the parent NN’s topology and/or weights.
- Apply the new population composed of the top performing NNs and their mutant offspring to the problem again.
- Finally, repeat steps 2-5 until some terminating condition is reached, where such a condition could be an emergence of a NN in the population with some fitness/performance level that you are seeking, or when the population is no longer generating better performing NNs, when evolution has stagnated.


### Fundamentals I
--------------------

A neuron is just a cell that can accept signals, and based on its chemical and geometrical properties, produce an output.

Artificial neural networks (NN), are simulated biological neural networks to different levels of precision. A typical artificial neuron, aka neurode, does not simulate a biological neuron at the atomic, or even molecular level. Artificial neurons are abstractions of biological neurons, they represent the essentials of biological neurons, their nonlinear signal integration, plasticity, and concurrency. 

We abstract the functionality undertaken by the receptors on the dendrites with simple weights, nevertheless, each incoming signal is weighted, and depending on whether the weight is positive or negative, each incoming signal can act as an excitatory or inhibitory one, respectively. We abstract spatiotemporal signal integration that occurs at the axon hillock with an activation function (which can be anything, and as complex as the researcher desires), nevertheless, the weighted signals are integrated at the output point of the artificial neuron to produce the final output vector, which is then passed onwards to other neurons. And finally, we abstract the functionality undertaken by the axon with simple signal message passing, nevertheless, the final output signal is propagated, diligently, to all postsynaptic artificial neurons. 

Both, biological and artificial neural networks are Turing complete, which means that both possess the same amount of flexibility. The implications of the fact that both systems are universal Turing machines is that even if a single artificial neuron does not do as much, or perform as a complex computation as a single biological neuron, we could put a few artificial neurons together into an artificial neural circuit, and this artificial neural circuit will have the same processing power and flexibility as a biological neuron.

The limits of speed, signal propagation, neural plasticity, life span of the neuron, integration of new neural systems over the organism’s lifetime, are all limited in wetware by biology. None of these limitations are present in hardware, the only speed limit of signal propagation is that of light in a hardware based neural computing system. The non biological neural computer can add new neural circuits to itself over lifetime, and that lifetime span is unlimited, given that hardware upkeep is possible.

Artificial neurons accept vector input signals, and output a vector signal of length 1. Each input signal is weighted; each element in the input vector is multiplied by a weight in a weight list associated with that input vector, and that particular element in the input vector. Thus, the integration of the incoming signals is done by calculating a dot product of the incoming vectors and the weight vectors associated with those input vectors.

The weight lists weigh the importance of each input vector. The way we integrate the input signal is by calculating a dot product of the weights and the input signals. Once the dot product is calculated, we compute the output of the neuron, Output = F(X), where F is the activation function, and X = Dot_Product + Bias. The neuron then packages this result into a vector of length 1, like so: [Output], and then fans out this output vector to the elements that it is connected to. 

A sigmoid function, or hyperbolic tangent, is the typically used activation function in artificial neurons. A multi-layered feed forward neural circuit composed of neurons using sigmoid activation functions can act as a universal function approximator, which means that a neural network composed of such neural circuits can do anything.

Now regarding the bias input, it is simply an input vector which is used to increase the flexibility of the neuron by giving it an extra weight that it can use to skew the dot product of the input signals. Not every neuron needs to have a bias input, it’s optional, and if the weight for the bias input is 0, then that is equivalent to a neuron that does not have a bias input at all. In essence, the bias controls how excitable in general the neuron is, whereas the weights of the non bias inputs control how significant those inputs are, and whether the neuron considers them excitatory or inhibitory. 

The main question though is, how do we figure out the synaptic weights and the NN topologies needed to solve some problem? The answer is, a learning algorithm, an automated algorithm that sets up the weights.

The process of training a neural network is accomplished by changing its weights and topology from the outside, by some algorithm external to the NN based system. On the other hand, a neural network is learning if it is adjusting and improving itself of its own volition, through its exposure to experience and the change of its NN topology and neural parameters. Neural plasticity is the ability of the neuron to change due to experience. Thus for example if we create a large NN system composed of plastic (those possessing plasticity) neurons, and then release it into a virtual environment and it improves on its behaviour, it learns how to survive in the environment through experience... that is what I would refer to as learning. This is called Unsupervised Learning.

Unsupervised learning refers to the problem of trying to determine structure in incoming, unlabelled data. In such a learning algorithm, because the input is unlabeled, there is no error or reward signals which can be used to guide the modification process of neural weights based on the difference between the output and the expected output. Instead, the NN self modifies its parameters based on the inputs and its own outputs through some algorithm. There are two general kinds of such learning algorithms; a learning algorithm can either be a system that has a global view of the NN, and which uses this global view to modify neural weights (kohonen, competitive…), or a learning algorithm can be a local one, embedded in each neuron and letting it modify its own synaptic weights based on its inputs and outputs (hebbian, modulated…). 

To create a truly intelligent neurocomputing system, we need to combine static neurons, neurons with plasticity, and different forms of unsupervised learning algorithms... all into a vast neural network. And combine it in a way that all these different parts work together perfectly, and allow for the whole emergent NN system to truly learn, which is the case with evolved biological neural networks. 

A NN is usually composed of multiple layers. The depth tells us the minimum amount of non parallel processing that has to be done by a distributed NN. Finally, assigning each neuron a layer allows us to see whether the connections from one neuron to another are feed forward, meaning some neuron A sends signals to a neuron B which is in front of neuron A, or whether the connection is recurrent, meaning some neuron A sends a signal to neuron B which itself is behind A, and whose original output signal is either fed directly to neuron A, or was forwarded to other neurons and then eventually got to neuron A before it itself produced its output signal (the recurrent signal that it sent back to neuron B). Indeed in recurrent NNs, one can have feedforward and feedback loop based neural circuits, and a neuron B could have sent a signal to neuron A, which then processed it and sent its output back to neuron B... When a neural network is composed of neurons whose output signals go only in the forward facing direction, such a network is called a feedforward NN. If the NN also includes some recurrent connections, then it is a recurrent NN. 

What is significant about recurrent neural networks is that they can form memory circuits. The neuron has memory of its previous action, and depending on the weight for that recurrent connection, its previous signal at time step T can play a large or a small part in its output at a time step T+1.

Thus, though it is the NN that thinks, it is the NN with sensors and actuators that forms the whole system. Without our sensory organs, our brain is in the dark, and without our muscles, it does not matter what we think, because we can have no affect on, and no way to interact with, the world. It is the same with artificial NNs. They require sensors, and actuators. A sensor can be a camera, which can package its output signals in a way that can be understood by the NN, for example by representing the sensory signals as vectors. An actuator can be a motor, with a function that can translate the NN’s output vector into electrical signals that controls the actual motor. Thus it is the whole thing, the sensors connected to and sending the sensory signals to the NN, and the NN connected to and sending its output signals to the actuators, that forms the full system. 

We will refer to such a complete and self contained system, the Sensors connected to the Neural Network, which itself is connected to Actuators, as the NN based system, or NN based agent.


### Fundamentals II
--------------------

Evolutionary computation algorithms are population based optimization algorithms inspired and derived from the Darwinian principles of biological evolution. Evolution, at its core, replication, creating copies, is really the main thing. Thus, to sum it up, an evolutionary process requires a population of agents and a way to represent their genotypes in the search space. It requires reproduction, a way for parents to create offspring by creating new variations of their own genotypes. It requires a mapping system, to convert the genotype (in the case of biological organisms, DNA & RNA) in the search space, to phenotypes (the actual organism, its morphology, and its behaviour) in the solution space. Finally, it requires a selection method, a way to choose the fit agents, a way to discern the fit from the unfit. Thus, evolution is a directed parallel search, directed by the principle of survival of the fittest, however that fitness is defined. 

The evolutionary process can be represented with the following set of steps: 
- Create a seed population of simple organisms. The organisms must have both, genotypic and phenotypic representations, and a function capable of mapping between the two. The genotype must have a form such that mutation operators can be applied to it, thus allowing the population of organisms to evolve.
- Create a fitness function, a function that can give each agent in the population a fitness score, such that the agents can be compared against one another, to see which is more fit. 
- DO: 
    - Evaluate each organism’s fitness in the population. Give each organism a fitness score using the fitness function, based on that organism’s performance. 
    - Choose the fit organisms within the population. 
    - Remove the unfit organisms from the population. 
    - Create offspring from the fit agent genotypes. The offspring are variations on the genotype of their fit parents. The offspring can be created through the process of cross over, mutation, or both. For example an offspring can be created by first creating a clone of a fit parent, and then applying various mutation operators to the clone, and thus generating a mutant of the fit parent. 
    - Compose the new population from the offspring and their parents (this method is called an elitist selection, because the fit organisms of each generation always survive to the next generation). 
- UNTIL: Termination condition is reached. Where that condition can be some goal fitness score, or some maximum amount of computational power or time expanded on the problem to which the evolutionary algorithm is being applied. 

The genotype is the representation, it is that to which we apply mutation operators, it is that string or program from which the actual phenotype of the organism can be derived. The genotype is a mutable encoding of the organism, a point in the search space of all possible genotypes. The phenotype is the actual characteristics of the organism, its behavior and morphological properties, a point on the solution space, of all possible solutions. Evolution occurs through the mutation of our genotype. It is the phenotype whose fitness is judged, it is the phenotype that interacts directly with the environment it inhabits. Evolution is the process undertaken by a population of agents. What is evolving is the population rather than an individual. 

You need only create a genotype encoding with a mapping to phenotype, a fitness function, a selection algorithm, and the mutation operators you wish to apply during the reproduction/offspring-creation phase to create variation in the offspring. The evolutionary process takes care of the rest. 

Today there are advanced versions of GA, GP, ES, EP, and MA. The lines between these flavors of evolutionary computation are blurred to the point where it’s difficult to pinpoint where one system ends and another begins. Indeed, they are all technically the same. After all, GP is just GA that operates on a tree algorithm. And if we add the ability to also mutate evolutionary strategy parameters, then we’ve just went from GA to ES. EP is just GA specialized for finite state machines, and Memetic Computing (MC) is simply a variation on the standard evolutionary computing, which splits the local and global search into separate phases...

Generational evolution simply means that we create a population of some size X of seed agents, apply them to some problem, wait until all agents in the population have been evaluated and given a fitness score, then select the best of the population, allow them to create offspring, and then create the next generation composed of the best agents of the previous generation plus their offspring, or some other appropriate combination of fit parents and newly created offspring. That is the essence of the standard generational evolution. 

The steady state evolution tries to emulate the biological world to a slightly greater degree. In this approach there is no wait for the entire population to be evaluated before a new agent is created. Instead, as soon as one agent has finished working on a problem (has been evaluated), or has perished or gathered enough resources (in the case of an ALife simulation), a new offspring is created. The new offspring is either created by some already existing agent through the execution of a create_offspring actuator, or is created by the neuroevolutionary system itself, after it has calculated what genotypes to use as the base for the offspring creation process, and whether the environment can support another agent. In this manner, the population size working on a problem, or existing in a simulated environment, is kept relatively constant. There are always organisms in the environment, when some die, new ones are created. There is a constant turnover of new agents and new genotypes and phenotypes in the population.


#### Sources
--------------------
G.I. Sher, Handbook of Neuroevolution Through Erlang. Springer 2012, print ISBN: 978-1-4614-4462-6 ebook ISBN: 978-1-4614-4463-6.