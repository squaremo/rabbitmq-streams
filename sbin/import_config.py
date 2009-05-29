import sys
import os.path
orig_path = os.path.dirname(sys.argv[0])
if len(orig_path) > 0:
    orig_path = orig_path + "/"
path = orig_path + "../harness/python/lib"
sys.path.insert(0, path)

if len(sys.argv) < 2:
    print "Usage: python import_config.py <directory>"
    sys.exit(0)

import couchdb
import glob
import string
try:
    import simplejson as json
except ImportError:
    import json

server = couchdb.Server("http://localhost:5984/")

for dbdir in glob.glob(sys.argv[1] + "/*"):
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
