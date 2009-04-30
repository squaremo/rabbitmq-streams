# Getting started

## Installation

The top-level makefile has targets for installing the dependencies. At
the minute it assumes you are using a Linux distribution with
`apt-get`; but this is restricted to a small, easily replicated
section of build dependencies.

Use `make setup` to install runtime dependencies.

## Running and debugging

There are three components which need to be running: RabbitMQ,
CouchDB, and the Orchestrator. RabbitMQ and CouchDB are together
referred to as the "core" and must be started before the
Orchestrator. The following is our preferred means to get everthing up
and running:

`make listen_all` : this starts up three xterms which are configured
to listen to the outputs of each component.

The following can be used to stop all the components (this doesn't
hurt if they're not already running), clean the CouchDB and RabbitMQ
broker, set them back up, install the test configuration and get
everthing up and running.

`make stop_all_nox all cleandb start_core_nox && sleep 2
 && ./setup-core.sh && sleep 1 && make start_orchestrator_nox
 && sleep 5 && cd sbin && python install_test_data.py && cd ..
 && make start_orchestrator_nox `

In general, Makefile targets that end in `_nox` (_No X_) will not
start new xterms.

All three components are normal Erlang shells (though due to
forwarding outputs over `nc`, the command history features are
lost). Thus to quit any of the components, enter `q().` and press
return in the shells. The Makefile targets take care of stopping the
components as necessary.

## Developing

TODO
