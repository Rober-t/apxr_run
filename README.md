# apxr_run

A topology and parameter evolving universal learning network originally created
by Gene Sher. See [Handbook of Neuroevolution Through Erlang](http://www.amazon.com/Handbook-Neuroevolution-Through-Erlang-Gene/dp/1461444624).

![img](https://github.com/Rober-t/apxr_run/blob/master/img.png)

--------------------
### Requirements

  - [asdf](https://asdf-vm.com/#/)
  - [CMake](https://cmake.org/)
  - [Rebar3](https://github.com/erlang/rebar3)

More information on installing Erlang/OTP & Rebar3 can be found [here.](https://www.rebar3.org/docs/getting-started)

--------------------
### Introduction

`apxr_run` is a distributed topology and weight evolving artificial neural
network originally created by Gene Sher. Significant changes are a deeper
integration of the OTP application structure, replacement of the single scape
process with a quad tree, converting the sensor -> scape and actuator -> scape
processes to be asynchronous, dropping Mnesia in favour of RocksDB and initial
work to integrate with Elixir.

![nn_system](https://github.com/Rober-t/apxr_run/blob/master/nn_system.png)

------------------
### How to:

#### 1. Build

    $ rebar3 compile

------------------

#### 2. Run Dialyzer (Erlang static analysis tool)

    $ rebar3 dialyzer

------------------

#### 3. Run Eunit (Unit tests)

    $ rebar3 do eunit, cover --verbose

------------------

#### 4. Run Xref (Cross reference analysis)

    $ rebar3 xref

------------------

#### 5. Run Checks

    $ rebar3 check

which runs the following:

```
[compile, xref, dialyzer, eunit]
```

------------------

#### 7. Run (development)

    $ rebar3 shell
    1> experiment_mgr:run().

------------------

#### 8. Generate release

    $ rebar3 release -d -p priv/lib/elixir/ebin/

------------------

#### 9. Run release (development)

    $ _build/default/rel/apxr_run/bin/apxr_run console

------------------

#### 10. Monitor

Development:
```erlang
% Erlang/OTP observer
1> observer:start().
```

Production:
```
% observer_cli
1> observer_cli:start().
```

------------------

#### 11. Conduct an experiment

The experiment_mgr process composes experiments by performing N evolutionary
runs, and then produces statistical data and chart ready files of the various
evolutionary dynamics and averages.

All these files can be found under the `/experiments` directory.

--------------------
### Configuration

Every application of the system needs a corresponding configuration file. The
`/config` directory contains a number of examples that can be used to get
started.

Remember, we do not need to force the system to use any one particular approach.
We can set it in the constraints to use all available functionality and the
system will evolve it all.

--------------------
### Benchmarks

Three benchmarks are included:
- Double pole balancing without damping
- Double pole balancing with damping
- Discrete T-maze

See the `/doc/examples/benchmarks.md` for more information.

--------------------
### Benchmarks

Two applications are included:
- Flatland
- FX

See the `/doc/examples/applications.md` for more information.

--------------------
### Documentation

##### Developers
The most useful resource for developers working on this project will be the
inline documentation.

##### General
For a more general introduction, see the `/docs.md` directory for a quick
introduction to neuroevolution and related topics.

For a more thorough introduction, the book "Handbook of Neuroevolution Through
Erlang" by Gene Sher is highly recommended. Indeed, much of the content in the
Docs section is taken from this book. As is the majority of the inline
documentation found in the source code. So, credit for the good stuff goes to
him.

--------------------
### Conventions & rules

Inaka's [erlang_guidelines](https://github.com/inaka/erlang_guidelines) should
be used as the basis for things that may be used as reason to reject a PR.

--------------------
### Future work

There are still many features that can be added to this system. You could make
the NNs modular, add Kohonen Map, Competitive NN, Hopfield Network, and other
types of self organizing networking based modules.

Optimizations could be made to separate the substrate into multiple parallel
hypercubes, or feed this vector based representation to a GPU, which could then
process it in parallel if implemented accordingly. It is possible to
significantly accelerate the iterative rule encoding by, for example, converting
the feedforward NNs into single functions, which can then be utilized
independently by each neurode. A feedforward NN is after all just a function of
functions, which can be represented in the form of: FF = f1(f2(f3(...)...)...),
with the FF function then used directly by the substrate embedded neurodes. This
would effectively make the entire SENN, excluding the sensors and actuators, be
represented by a single process. By transforming the evolved feedforward NNs
into single functions, then embedding those functions within each neurode, you
are thus allowing each neurode to simply call on it as if it were a simple
plasticity function.

New mutation operators could be added, for example, pruning or the use of
splitting. Committee machines using dead_pools could be added too.

You could of course create new fitness functions that take into account the
Cartesian distance of the connections between the neurodes within the substrate,
for example, which would allow you to push for closely connected neural clusters.

Additionally, a "crystallization" feature could be added where neural circuits
which have been topologically stable during the NN's evolution are crystallized
into a single function, a single processes represented module. New types of
signal normalization preprocessors can be added as well.

Lastly, the project could be interfaced with other languages to optimize for
speed or to take advantage of various deep network engines.

--------------------
### Related publications

1. [Handbook of Neuroevolution Through Erlang](http://www.amazon.com/Handbook-Neuroevolution-Through-Erlang-Gene/dp/1461444624) _by Gene Sher_.
2. [Agent-Based Modeling Using Erlang](https://pdfs.semanticscholar.org/239e/e207f97233f3e28852fe43341aaaaf4bb2e7.pdf) _by Gene Sher_.

--------------------

Copyright (C) 2018 - 2019 ApproximateReality - approximatereality@gmail.com
