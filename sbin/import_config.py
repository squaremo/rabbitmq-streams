import sys
import os.path
orig_path = os.path.dirname(sys.argv[0])
if len(orig_path) > 0:
    orig_path = orig_path + "/"
path = orig_path + "../harness/python/lib"
sys.path.insert(0, path)

import couchdb
import glob
import string
try:
    import simplejson as json
except ImportError:
    import json


if len(sys.argv) != 3:
    print "Usage: python import_config.py <couch_db> <directory>..."
    sys.exit(41)

couch_url, dirs_to_import = sys.argv[1], sys.argv[2:]
server = couchdb.Server(couch_url)

for dir_to_import in dirs_to_import:
    if not os.path.exists(dir_to_import):
        print >> sys.stderr, "No such dir:", dir_to_import
        sys.exit(44)
    print 'Processing', dir_to_import
    for dbdir in glob.glob(os.path.join(dir_to_import, "*")):
        dbname = os.path.split(dbdir)[1]
        try:
            db = server.create(dbname)
        except couchdb.PreconditionFailed:
            db = server[dbname]
        for docfilename in glob.glob(os.path.join(dbdir, "*.js")):
            docid = string.replace(os.path.splitext(os.path.basename(docfilename))[0], '.', '_')
            d = json.load(open(docfilename))
            if docid in db:
                print 'Updating', docid
                d['_id'] = db[docid]['_id']
                d['_rev'] = db[docid]['_rev']
            else:
                print 'Inserting', docid
            db[docid] = d
