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

for dbdir in glob.glob("test_data/*"):
    dbname = os.path.split(dbdir)[1]
    try:
        db = server[dbname]
    except couchdb.ResourceNotFound:
        db = server.create(dbname)
    for docfilename in glob.glob(dbdir + "/*.js"):
        docid = os.path.splitext(os.path.basename(docfilename))[0]
        f = open(docfilename)
        db[docid] = json.loads(f.read())
        f.close()
