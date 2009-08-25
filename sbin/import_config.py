#!/usr/bin/python -Wignore::DeprecationWarning

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

from optparse import OptionParser

parser = OptionParser(usage="usage: %prog [options] DIR ...")
parser.add_option('--couchdb',
                  default="http://localhost:5984/", dest="couchdb",
                  help="URL of CouchDB into which to import")
parser.add_option('--flatten', '-f',
                  default=False, action='store_true', dest='flatten',
                  help="Treat the given directories as databases," \
                  "rather than their subdirectories as databases")
(options, args) = parser.parse_args()

if len(args) < 1:
    parser.print_help()
    sys.exit(41)

couch_url, flatten, dirs_to_import = options.couchdb, options.flatten, args
server = couchdb.Server(couch_url)

def import_dir(dbdir):
    dbname = os.path.split(dbdir)[1]
    print 'Database %s' % dbname
    try:
        db = server.create(dbname)
    except couchdb.PreconditionFailed:
        db = server[dbname]
    for docfilename in glob.glob(os.path.join(dbdir, "*.js")):
        docid = string.replace(os.path.splitext(os.path.basename(docfilename))[0], '.', '_')
        d = json.load(open(docfilename))
        if docid in db:
            print '  Updating', docid
            d['_id'] = db[docid]['_id']
            d['_rev'] = db[docid]['_rev']
        else:
            print '  Inserting', docid
        db[docid] = d

for dir_to_import in dirs_to_import:
    if not os.path.exists(dir_to_import) or not os.path.isdir(dir_to_import):
        print >> sys.stderr, "No such dir:", dir_to_import
        sys.exit(44)
    print 'Processing', dir_to_import

    if flatten:
        import_dir(dir_to_import)
    else:
        for dbdir in glob.glob(os.path.join(dir_to_import, "*")):
            if not(os.path.isdir(dbdir)): continue
            import_dir(dbdir)
