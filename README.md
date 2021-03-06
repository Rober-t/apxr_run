# apxr_run

A topology and parameter evolving universal learning network originally created
by Gene Sher.

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

------------------
### How to:

#### 1. Build

    $ rebar3 compile

#### 2. Run Dialyzer (Erlang static analysis tool)

    $ rebar3 dialyzer

#### 3. Run Eunit (Unit tests)

    $ rebar3 do eunit, cover --verbose

#### 4. Run Xref (Cross reference analysis)

    $ rebar3 xref

#### 5. Run

    $ rebar3 shell
    1> experiment_mgr:run().

#### 6. Generate release

    $ rebar3 release -d -p priv/lib/elixir/ebin/

#### 7. Run release

    $ _build/default/rel/apxr_run/bin/apxr_run console

#### 8. Observe

```
1> observer:start().
```

or

```
1> observer_cli:start().
```

#### 9. Conduct an experiment

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
### Applications

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
For a more general introduction, see the `/docs` directory for a quick
introduction to neuroevolution and related topics.

For a more thorough introduction, the book "Handbook of Neuroevolution Through
Erlang" by Gene Sher is highly recommended. Indeed, much of the content in the
Docs section is taken from this book as is the majority of the inline
documentation found in the source code. So, credit for the good stuff goes to
him.

--------------------
### Conventions & rules

Inaka's [erlang_guidelines](https://github.com/inaka/erlang_guidelines) should
be used as the basis for things that may be used as a reason to reject a PR.

--------------------
### Related publications

1. [Handbook of Neuroevolution Through Erlang](http://www.amazon.com/Handbook-Neuroevolution-Through-Erlang-Gene/dp/1461444624) _by Gene Sher_.
2. [Agent-Based Modeling Using Erlang](https://pdfs.semanticscholar.org/239e/e207f97233f3e28852fe43341aaaaf4bb2e7.pdf) _by Gene Sher_.

--------------------

Copyright (C) 2018 - 2019 ApproximateReality - approximatereality@gmail.com
