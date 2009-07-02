import sys, os.path
path = os.path.dirname(sys.argv[0])
if len(path) > 0:
    path = path + "/"
path = path + "../harness/python/lib"
sys.path.insert(0, path)

import amqplib.client_0_8 as amqp
import glob
try:
    import simplejson as json
except ImportError:
    import json

connection = amqp.Connection(host="localhost:5672", userid="feedshub_admin", password="feedshub_admin")
channel = connection.channel()

def subscribe(feed, outcomp, outname, incomp, inname):
    def printMsg(msg):
        print "Message in %s:" % feed
        print "%s/%s -> %s/%s" % (outcomp, outname, incomp, inname)
        print msg.body
    exchange='%s_%s_%s' % (feed, outcomp, outname)
    q, mc, cc = channel.queue_declare()
    channel.queue_bind(q, exchange)
    channel.basic_consume(q, callback=printMsg)

print "Active component configs:"
for configdoc in glob.glob("test_data/feedshub_config/*"):
    config = json.loads(open(configdoc).read())
    configname = os.path.splitext(os.path.basename(configdoc))[0]
    if config['active']:
        print "Feed %s is active" % configname
        nodes = config['wiring']['nodes']
        for node in nodes:
            plugindoc = 'plugins/%s/plugin.js' % (nodes[node]['type'])
            print "Loading plugin for %s (%s)" % (nodes[node]['type'], plugindoc)
            plugin = json.loads(open(plugindoc).read())
            print "Component %s_%s" % (configname, node)
            for inp in plugin['inputs']:
                print "  Input queue: %s_%s_%s" % (configname, node, inp['name'])
            for outp in plugin['outputs']:
                print "  Output exchange: %s_%s_%s" % (configname, node, outp['name'])
        for edge in config['wiring']['edges']:
            subscribe(configname, *edge)
    else:
        print "Feed %s not active" % configname

while True:
    channel.wait()
