import sys
sys.path.append("harness/python/lib")

import couchdb
import os.path
import glob
try:
    import json
except ImportError:
    import simplejson as json

server = couchdb.Server("http://localhost:5984/")

for dbdir in glob.glob("test_data/feedshub_config/*"):
    dbname = os.path.splitext(os.path.basename(dbdir))[0]
    try:
        print "Deleting feed_%s" % dbname
        del server['feed_%s' % dbname]
    except:
        pass
    for db1 in filter(lambda name: name.startswith(dbname), server):
        print "Deleting %s" % db1
        del server[db1]
