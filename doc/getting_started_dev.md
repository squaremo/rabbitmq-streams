# Getting started

## Installation

The top-level makefile has targets for installing the dependencies. At
the minute it assumes you are using a Linux distribution with
`apt-get`; but this is restricted to a small, easily replicated
section of build dependencies.

Use `make setup` to install runtime dependencies.

## Running and debugging

`make run_core` will run RabbitMQ and CouchDB and configure them for
our needs.  `make run` starts the orchestrator in a new xterm; or,
`make -C orchestrator run` will start in the current console.

TODO ways and means of debugging ..

## Developing

TODO
