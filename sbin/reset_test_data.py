import sys
import os.path
orig_path = os.path.dirname(sys.argv[0])
if len(orig_path) > 0:
    orig_path = orig_path + "/"
path = orig_path + "../harness/python/lib"
sys.path.insert(0, path)

import couchdb
import glob
try:
    import json
except ImportError:
    import simplejson as json

server = couchdb.Server("http://localhost:5984/")

for dbdir in glob.glob(orig_path + "../test_data/feedshub_status/*"):
    dbname = os.path.splitext(os.path.basename(dbdir))[0]
    try:
        print "Deleting feed_%s" % dbname
        del server['feed_%s' % dbname]
    except:
        pass
    for db1 in filter(lambda name: name.startswith(dbname), server):
        print "Deleting %s" % db1
        del server[db1]
