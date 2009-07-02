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

connection = amqp.Connection(host="localhost:5672", userid="feedshub_admin", password="feedshub_admin")
channel = connection.channel()
exchange = "feedshub/config"

channel.basic_publish(amqp.Message(body="install views", children=None), exchange=exchange)
