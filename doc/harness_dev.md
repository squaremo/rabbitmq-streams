
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
