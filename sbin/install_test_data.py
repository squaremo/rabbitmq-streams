import sys
sys.path.append("../harness/python/lib")

import couchdb
import os.path
import glob
import string
try:
    import simplejson as json
except ImportError:
    import json

server = couchdb.Server("http://localhost:5984/")

for dbdir in glob.glob("../test_data/*"):
    dbname = os.path.split(dbdir)[1]
    try:
        db = server.create(dbname)
    except couchdb.PreconditionFailed:
        db = server[dbname]
    for docfilename in glob.glob(dbdir + "/*.js"):
        docid = string.replace(os.path.splitext(os.path.basename(docfilename))[0], '.', '_')
        f = open(docfilename)
        d = json.loads(f.read())
        f.close()
        if docid in db:
            print 'Updating', docid
            d['_id'] = db[docid]['_id']
            d['_rev'] = db[docid]['_rev']
        else:
            print 'Inserting', docid
        db[docid] = d
