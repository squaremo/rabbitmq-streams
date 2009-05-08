import sys
import os

here = os.path.dirname(os.path.abspath(sys.argv[0]))
sys.path.insert(0, os.path.join(here, '../harness/python'))
sys.path.insert(0, os.path.join(here, '../harness/python/lib'))

import amqplib.client_0_8 as amqp
import feedshub as fh
from feedshub import json

pluginname = sys.argv[1]
config = (len(sys.argv) > 2) and open(sys.argv[2]).read() or "{}"

print "Configuration: "
print config

config = json.loads(config)

plugindir = os.path.join(here, '..', 'plugins', pluginname)
pluginjsfile = open(os.path.join(plugindir, 'plugin.js'))
plugin = json.loads(pluginjsfile.read())
pluginjsfile.close()

print "Plugin descriptor:"
print plugin

connection = amqp.Connection(host='localhost:5672',
                             userid='feedshub_admin',
                             password='feedshub_admin',
                             virtual_host='/')

channel = connection.channel()

def newname():
    import sha
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

def talker(queue):
    exchange = declexchange()
    channel.queue_bind(queue, exchange)
    def say(something):
        channel.basic_publish(amqp.Message(body=something), exchange)
    return say

talkers = dict((name, talker(queue)) for (name, queue) in inputs.items())

def subscribe(name, exchange, key=''):
    def out(msg):
        print ("%s: %s" % (name, msg.body))
    queue = declqueue()
    print "%s bound to %s" % (name, queue)
    channel.queue_bind(queue, exchange, routing_key=key)
    channel.basic_consume(queue, no_ack=True, callback=out)

for (name, exchange) in outputs.items():
    subscribe(name, exchange)

subscribe("log", "feedshub/log", '#')

import threading
class ListenerThread(threading.Thread):
    def run(self):
        while True:
            channel.wait()
listen = ListenerThread()
listen.daemon = True
listen.start()

# Assemble the plugin init
init = {
    "harness_type": plugin['harness'], # String from harness in plugin.js
    "plugin_name": pluginname,  # String from type in nodes in wiring in feed config
    "plugin_dir": os.path.abspath(plugindir),
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
    "state": "http://localhost:5984/plugin_test_harness/state", # %%% MAKE THIS
    # the document within the feed state database that can be used to store state
    "database": None # %%% MAKE THIS
}

harnessdir = os.path.join(here, '..', 'harness', plugin['harness'])
harness = os.path.join(harnessdir, 'run_plugin.sh')

import subprocess
pluginproc = subprocess.Popen([harness], cwd=harnessdir, stderr=subprocess.STDOUT, stdin=subprocess.PIPE)
print "Initialising plugin with:"
print json.dumps(init)
pluginproc.stdin.write(json.dumps(init)); pluginproc.stdin.write("\n")

print
print "Listening on %s:" % inputs.keys()
while True:
    line = sys.stdin.readline()
    bits = line.split(":")
    talkername = bits[0]
    #print talkername
    talkers[talkername]("".join(bits[1:]))

