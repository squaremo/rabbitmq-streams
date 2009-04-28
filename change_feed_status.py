import sys, os.path, string
sys.path.append("harness/python/lib")

try:
    import json
except ImportError:
    import simplejson as json

import amqplib.client_0_8 as amqp
import couchdb.client as couch

connection = amqp.Connection(host="localhost:5672", userid="feedshub_admin", password="feedshub_admin")
channel = connection.channel()
exchange = "feedshub/config"

feedId = sys.argv[1]
feedStatus = False
if string.lower(sys.argv[2]) in ["true", "on", "activate", "yes", "indubitably", "positive", "1"]:
    feedStatus = True

url = "http://localhost:5984/feedshub_status/" + feedId + "_status"
feedStatusResource = couch.Resource(None, url)

try:
    feedStatusResource.head()
except:
    print "Unable to find the status for feed id " + feedId
    sys.exit(1)

resp, data = feedStatusResource.get()
statusDoc = couch.Document(data)

oldActive = statusDoc['active']
statusDoc['active'] = feedStatus
feedStatusResource.put(content = statusDoc)

channel.basic_publish(amqp.Message(body="status change", children=None), exchange=exchange, routing_key=feedId)
