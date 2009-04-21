
# Plugin harness development

Each environment (e.g., Java, Python) in which plugins run needs a
harness.  Minimally, this is simply a shell script that starts a
plugin process given a plugin name.

The harness also provides some abstraction of the services needed by
plugins; e.g., hooking up communications channels, storing documents.
This abstraction -- a base class, say -- encapsulates the conventions
for how plugins are initialised, communicated with, and so on, letting
the plugin developer be concerned only with the specific task of the
plugin.

The set of harness and plugin conventions is currently a moving
target; however, in general, the Python and Java harnesses (and this
document) will be kept up-to-date.

## Harness invocation

The harness is named by the plugin descriptor `plugin.js` in the
plugin directory.  The name is treated as a directory under
`harness/`, and the file `run_plugin.sh` in that directory is invoked.
The plugin configuration is then printed, as JSON, to that process's
STDIN.

The harness, then, must _at least_ read the configuration, extract the
plugin name (and use it as a directory under `plugins/`), and run the
plugin code, supplying the configuration in an appropriate form.  It
may also need to set environment variables, load modules, and so on.

Each harness will have its own convention for how to run a plugin
given its name.  For example, the Python harness treats the plugin
name as the directory **and** as a module name, under which it (by
convention) expects to find a callable named `run`, which it invokes
with the arguments as a dictionary.  It also puts the harness
directory on the `PYTHON_PATH` so that the plugin base class can be
imported, as well as `lib/` in the plugin directory; and, it changes
the working directory to the plugin directory so that resources can be
loaded relative to that directory.

## Harness services

The harness also provides convenience APIs for interacting with the
system. In principle, following the invocation convention -- e.g., for
Python, providing a correctly-named module with a run(args) procedure
-- is enough. But many details of the configuration can be taken care
of for the plugin developer.

### Instance configuration

An instance of the plugin may have configuration specific to that
instance. (This is due to be tidied up)

This is supplied by the orchestrator, and should be exposed
read-only to the plugin code.

### Channels

The plugin descriptor, `plugin.js`, specifies named input and output
channels required by an instance of the plugin. E.g.,

    ...
    "inputs": [{"name": "in"}],
    "outputs": [{"name": "result"}],
    ...

The orchestrator constructs input channels as AMQP queues, and output
channels as AMQP exchanges. The names of these queues and exchanges
are supplied as part of the initialisation configuration; e.g.,

    ...
    "inputs" : ["transformer_in"],
    "outputs" :["transformer_result]
    ...

Note that the queue and exchange names will in general be arbitrary,
and that they are supplied in an ordered list.  The harness must refer
to the plugin descriptor to match the queue or exchange to the named
channel.

Giving the plugin programmer access to the channels in a convenient
way will depend on the capabilities of the environment. The Python
harness lets the plugin developer supply a maps of channel names to
method names; input channels use the named method as a callback, and
output channels are inserted into the object as methods.

#### Notification channel

TODO yet to be implemented

### State

A plugin instance gets a document in which to store its running
state. This state will persist over restarts, and will be visible to
management interfaces. It should be exposed as read-write.

TODO Avoiding conflicts -- maybe the state is the argument and result
of any callback (and these are serialised)?

### Storage

The plugin descriptor can also specify a storage database private to
each instance. The orchestrator provides the name of this database in
the initialisation configuration.

TODO safe ways of exposing this to the plugin developer.
