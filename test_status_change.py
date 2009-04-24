import sys, os.path
sys.path.append("harness/python/lib")

try:
    import json
except ImportError:
    import simplejson as json

import amqplib.client_0_8 as amqp

connection = amqp.Connection(host="localhost:5672", userid="feedshub_admin", password="feedshub_admin")
channel = connection.channel()

exchange = "feedshub/config"

def updateFeedStatus(feedId):
    channel.basic_publish(amqp.Message(body="status change", children=None), exchange=exchange, routing_key=feedId)

updateFeedStatus(sys.argv[1])
