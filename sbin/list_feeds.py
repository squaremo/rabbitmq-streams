#!/usr/bin/python -Wignore::DeprecationWarning

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

import couchdb.client as couch

from optparse import OptionParser

kinds = ['feeds', 'terminals', 'servers']

parser = OptionParser(usage="usage: %%prog [options] (%s) [ID ...]" % '|'.join(kinds))
parser.add_option("--dump",
                  default=False,
                  action="store_true",
                  dest="dump",
                  help="Ouput config of items")
parser.add_option("--active",
                  default=False,
                  action="store_true",
                  dest="active_only",
                  help="Show active items only")
parser.add_option("--couchdb",
                  default="http://localhost:5984/",
                  dest="couchdb",
                  help="CouchDB holding configuration")

(options, args) = parser.parse_args()

if len(args) < 1 or args[0] not in kinds:
    parser.print_help()
    sys.exit(0)

view = '_design/feeds/_view/join'
view_arg = string.lower(args[0])
entry_type = 'feed'

if view_arg == 'feeds':
    view = '_design/feeds/_view/join'
    entry_type = 'feed'
elif view_arg == 'servers':
    view = '_design/servers/_view/join'
    entry_type = 'server'
elif view_arg == 'terminals':
    view = '_design/terminals/_view/join'
    entry_type = 'terminal'

args = args[1:]
entry_status = entry_type + '-status'

if options.active_only:
    view = view + "_active"

couchdb = options.couchdb
if couchdb[-1] == '/':
    couchdb = couchdb[:-1]
db = couch.Database("%s/feedshub_status" % couchdb)
for row in db.view(view, group=True):
    if row.value != None:
        doc_id = row.value[entry_type]['_id']
        if len(args) > 0 and not doc_id in args:
            continue
        if options.dump:
            s = json.dumps(row.value, sort_keys=True, indent=4)
            print '\n'.join([l.rstrip() for l in  s.splitlines()])
        else:
            active = 'inactive'
            if row.value[entry_status]['active']:
                active = 'active'
            print row.value[entry_type]['_id'] + '\t' + active

