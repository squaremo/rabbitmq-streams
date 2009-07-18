#!/usr/bin/python -Wignore::DeprecationWarning

import sys, os.path, string
path = os.path.dirname(sys.argv[0])
if len(path) > 0:
    path = path + "/"
path = path + "../harness/python/lib"
sys.path.insert(0, path)

try:
    import simplejson as json
except ImportError:
    import json

import amqplib.client_0_8 as amqp
import couchdb.client as couch
from optparse import OptionParser, OptionGroup

parser = OptionParser(usage="usage: %prog [options] ID (on|off)")

parser.add_option("--couchdb",
                  default="http://localhost:5984/",
                  dest="couchdb",
                  help="CouchDB holding configuration")

amqpoptions = OptionGroup(parser, "AMQP connection parameters")
amqpoptions.add_option("--amqp-host", default="localhost:5672")
amqpoptions.add_option("--amqp-user", default="guest")
amqpoptions.add_option("--amqp-passwd", default="guest")
amqpoptions.add_option("--amqp-vhost", default="/")
parser.add_option_group(amqpoptions)

(options, args) = parser.parse_args()

if len(args) < 2:
    parser.print_help()
    sys.exit(0)
    
try:
    connection = amqp.Connection(host=options.amqp_host,
                                 userid=options.amqp_user,
                                 password=options.amqp_passwd)
except:
    print "Could not connect to message broker"
    sys.exit(0)

channel = connection.channel()

exchange = "feedshub/config"


feedId = args[0]
feedStatus = False
if string.lower(args[1]) in ["true", "on", "activate", "yes", "indubitably", "positive", "1", "start"]:
    feedStatus = True

docId = feedId
if '.' in docId:
    lst = docId.split('.')
    docId = lst[len(lst) - 1]

couchhost = options.couchdb
if couchhost[-1]=='/': couchhost = couchhost[:-1]
db = couch.Database('%s/feedshub_status/' % options.couchdb)
statusDoc = db.get(docId + "_status")

if statusDoc == None:
    print "Unable to find the status for feed id " + docId
    sys.exit(1)

routingkey=feedId
if statusDoc['type']=='terminal-status':
    configDoc = db.get(feedId)
    routingkey = ''
    for server in configDoc['servers']:
        routingkey += '%s.' % server['server']
    routingkey += '%s' % feedId

statusDoc['active'] = feedStatus
db.update([statusDoc])

channel.basic_publish(amqp.Message(body="status change", children=None), exchange=exchange, routing_key=routingkey)
