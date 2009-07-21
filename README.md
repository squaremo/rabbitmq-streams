# What is this?

"RabbitMQ Streams" is our name for the open source project developed
with the BBC to power the "BBC Feeds Hub".  It is a **data streams
management system**.

For background information, see

- [Introducing BBC Feeds Hub (BBC
Radiolabs)](http://www.bbc.co.uk/blogs/radiolabs/2009/04/introducing_bbc_feeds_hub.shtml)
- [Untangling the BBC’s data feeds](http://www.lshift.net/blog/2009/05/08/untangling-the-bbcs-data-feeds)
- [BBC Feeds Hub primer](http://www.lshift.net/blog/2009/07/13/bbc-feeds-hub-slides-from-london-erlang-factory)

You can think of RabbitMQ Streams as a distributed, robust, scalable,
secure, user-friendly and manageable version of Unix pipes.

The basic logical building blocks are *Sources* and *Destinations* of
data and *Pipelines*.  The latter are composed of *PipelineComponents*
which can route (e.g. based on regexp matches on Atom feed entries),
merge and transform the data in arbitrary ways.

Data arrive at sources, and leave from destinations, via *Gateways*,
which talk various protocols to the outside world.

Gateways as well as pipeline components (jointly referred to as
*Plugins*) can currently be written in Java and Python, and require little
 boilerplate (see
e.g. [regexp_replace.py](plugins/regexp_replace/regexp_replace.py
"regexp_replace.py") Support for other languages can be added
straightforwardly by creating a *Harness*; plugins are essentially just
programs following a simple protocol, with the harness taking care of
much of the detail.

The message wiring and plugin processes are managed by an Erlang/OTP
application called the *Orchestrator*.  This is in a sense the core of
Streams.

# Current state

We're still actively developing this towards a production release;
there are rough edges and of course, plenty of things we'd like to
improve.

# Getting started

The Makefile has a number of targets useful for development.

`make setup` installs build dependencies and sets up a development
environment.  This is geared towards `apt-get`, but it's fairly easy
to do the equivalent for e.g., macports.

We also need RabbitMQ and CouchDB; `make setup` builds these from
source in `build/opt`; `make install-dev-debs` will just install build
dependencies without building RabbitMQ and CouchDB.

`make all` builds the things .  This currently relies on a Maven
repository for the Java harness and plugins, which we'll make
available.

`make create-fresh-accounts` will install a minimal configuration, and
create a user in RabbitMQ for Streams to use.

Descriptions of the model items (sources, gateways, etc.) are kept in
CouchDB. `sbin/import_config.py DIR` can be used to import whole
configurations at once; e.g., `sbin/import_config.py
examples/showandtell_demo`.

`make run` will start RabbitMQ and CouchDB from the local builds,
start the development code, and tail the logs for you.

`make listen-orchestrator start-orchestrator-nox` starts *just* the
Streams orchestrator and tail its log in an xterm.

There is a more detailed "Getting started" guide in
[doc/getting\_started\_dev.org](doc/getting_started_dev.org).
