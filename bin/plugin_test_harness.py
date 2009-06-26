from __future__ import with_statement
import atexit
import httplib
import os
import re
import sha
import subprocess
import sys
import threading

here = os.path.dirname(os.path.abspath(sys.argv[0]))
sys.path.insert(0, os.path.join(here, '../harness/python'))
sys.path.insert(0, os.path.join(here, '../harness/python/lib'))

import amqplib.client_0_8 as amqp

import feedshub as fh
from feedshub import json

BAD_SYSTEM_STATE, BAD_CONFIG, BAD_CHANNEL, MALFORMED_INPUT = (2**n for n in range(4))
IO_LINE_REX=re.compile(r'^([<>]?)(\w*)\s*:(.*\n?)') # TODO(alexander): cut'n pasted

def json_repr(py_obj):
    # replace None w/ 0 to get indentation
    return json.dumps(py_obj, indent=None).replace('\n', '\n\t:')

config = json.loads((len(sys.argv) > 2) and open(sys.argv[2]).read() or "{}")

print "<$Configuration: ", json_repr(config)
print

plugindir = os.path.abspath(sys.argv[1])
with open(os.path.join(plugindir, 'plugin.js')) as f:
    plugin = json.loads(f.read())
print "## Plugin descriptor:"
print "#", json_repr(plugin)
connection = amqp.Connection(host='localhost:5672',
                             userid='feedshub_admin',
                             password='feedshub_admin',
                             virtual_host='/')

channel = connection.channel()

def newname():
    return 'test/%s' % sha.new(os.urandom(8)).hexdigest()

def declexchange():
    name = newname()
    channel.exchange_declare(name, 'fanout')
    return name

def declqueue():
    name = newname()
    channel.queue_declare(name)
    return name

outputspec = plugin['outputs_specification']
outputs = dict((spec['name'], declexchange()) for spec in outputspec)

inputspec = plugin['inputs_specification']
inputs = dict((spec['name'], declqueue()) for spec in inputspec)

# Now, we want to *listen* to the outputs, and *talk* to the inputs

def stdout_msg_outputter(msg):
    line_sep = "\n%s :" % (' ' * len(name))
    print (">%s:%s" % (name, line_sep.join(msg.body.split('\n')+[])))


def subscribe(name, exchange, key='', msg_outputter=stdout_msg_outputter):
    queue = declqueue()
    print "# %s bound to %s" % (name, queue)
    channel.queue_bind(queue, exchange, routing_key=key)
    channel.basic_consume(queue, no_ack=True, callback=stdout_msg_outputter)

for (name, exchange) in outputs.items():
    subscribe(name, exchange)

subscribe("log", "feedshub/log", '#')

def talker(queue):
    exchange = declexchange()
    channel.queue_bind(queue, exchange)
    def say(something):
        channel.basic_publish(amqp.Message(body=something), exchange)
    return say

talkers = dict((name, talker(queue)) for (name, queue) in inputs.items())

class ListenerThread(threading.Thread):
    def run(self):
        while True: channel.wait()

listener = ListenerThread()
listener.daemon = True
listener.start()

def init_couch_state():
    couch = httplib.HTTPConnection("localhost", 5984)
    #TODO(alexander): this is stupid, what it should really be doing is create
    #an unique id; unfortunately the dreaded document update conflict needs to
    #be resolved first
    for req, allowable in [(("DELETE", "/plugin_test_harness"), range(600)),
                           (("PUT", "/plugin_test_harness"), range(400))]:
        couch.request(*req)
        try:
            ans = couch.getresponse()
            ans_s = ans.read()
        except Exception, msg:
            print >> sys.stderr, (
                "Couldn't successfully talk to CouchDB: %s -> %s" % (req, msg))
            sys.exit(BAD_SYSTEM_STATE)
        if ans.status not in allowable:
            print >> sys.stderr, (
                "Could successfully talk to CouchDB: %s -> %3d: %s" % (
                req, ans.status, ans_s))
            sys.exit(BAD_SYSTEM_STATE)

init_couch_state()

statedocname = newname()

# Assemble the plugin init
init = {
    "harness_type": plugin['harness'], # String from harness in plugin.js
    "plugin_name": os.path.basename(plugindir),  # String from type in nodes in wiring in feed config
    "plugin_dir": plugindir,
    "feed_id": "test",
    "node_id": "plugin",
    "plugin_type": plugin,
    "global_configuration": {}, # place holder - currently we don't know where the values come from
    "configuration": config, # this comes from the feeds config, the node in the wiring
    "messageserver":  {"host": "localhost",
                       "port": 5672,
                       "virtual_host": "/",
                       "username": "feedshub_admin",
                       "password": "feedshub_admin"
                       },
    "inputs":  inputs, # Q name provided by orchestrator
    "outputs": outputs, # Exchange name provided by orchestrator
    "state": "http://localhost:5984/plugin_test_harness/%s" % statedocname,
    "database": "http://localhost:5984/plugin_test_harness"
}

harnessdir = os.path.join(here, '..', 'harness', plugin['harness'])
harness = os.path.join(harnessdir, 'run_plugin.sh')

pluginproc = subprocess.Popen([harness], cwd=harnessdir,
                              stderr=subprocess.STDOUT, stdin=subprocess.PIPE)
atexit.register(pluginproc.wait) # make sure we don't exit & leave this around!

print
print "## Initialising plugin with:"
print "#", json.dumps(init)
pluginproc.stdin.write(json.dumps(init)); pluginproc.stdin.write("\n")

print
print """##The input channels are: %s
# type 'INPUT:message' to inject a message into channel INPUT,
# type ':more-stuff' to continue the message and
# type '' (an empty line) to finish it (all newlines but the last are kept).
# type ^D to abort. You are free to insert whitespace before the ':'.
---
""" % ' '.join(inputs)

# TODO: a proper state-machine abstraction might be good here...
def repl(lines, talkers=talkers):
    WANT_CHANNEL, WANT_ANY = 'want_channel', 'want_any'

    channel = None
    talker_name, msg, state = None, None, WANT_CHANNEL
    exit_code = 0

    def ship():
        try:
            to_say = msg.rsplit('\n',1)[0]
            ## print >>sys.stderr, "A'shippin': %r"% to_say
            talkers[talker_name](to_say)
            return 0
        except KeyError:
            print >>sys.stderr, ("ERROR(BAD_CHANNEL): %r unknown" %
                                 talker_name)
            return BAD_CHANNEL

    for line in lines:
        if line.startswith('#'):
            pass
        elif not line.strip():
            if state == WANT_ANY:
                exit_code |= ship()
                talker_name, msg, state = None, None, WANT_CHANNEL
        else:
            try:
                typ, channel, bit = IO_LINE_REX.match(line).groups()
            except Exception:
                print >>sys.stderr, (
                    ">ERROR(MALFORMED_INPUT):"
                    " Bad input! (neither comment nor message):%r" % line)
                continue
            if not channel:
                if state is WANT_CHANNEL:
                    print >>sys.stderr, (
                        ">ERROR(MALFORMED_INPUT):"
                        " need some channel to send message to!")
                    exit_code |= MALFORMED_INPUT
                    continue
                elif not bit and not typ:
                    exit_code |= ship()
                    talker_name, msg, state = None, None, WANT_CHANNEL
                elif not typ:
                    msg += bit
                    state = WANT_ANY
                else:
                    print >>sys.stderr, (
                        ">ERROR(MALFORMED_INPUT):"
                        "%r on its own is not meaningful" % bit)
                    exit_code |= MALFORMED_INPUT
                    continue
            else:
                if typ != '>':
                    if state is WANT_ANY:
                        exit_code |= ship()
                    talker_name = channel
                else:
                    talker_name = None # FIXME
                msg = bit
                state = WANT_ANY
    if talker_name is not None:
        exit_code |= ship()
    return exit_code

sys.exit(repl(iter(sys.stdin.readline, '')))

