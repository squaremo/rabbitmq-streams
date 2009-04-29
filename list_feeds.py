import sys, os.path, string
sys.path.append("harness/python/lib")

try:
    import json
except ImportError:
    import simplejson as json

import couchdb.client as couch

view = '_design/feeds/_view/join'
if len(sys.argv) > 1 and string.lower(sys.argv[1]) == "active":
    view = '_design/feeds/_view/join_active'

db = couch.Database('http://localhost:5984/feedshub_status/')
for row in db.view(view, group=True):
    if row.value != None:
        s = json.dumps(row.value, sort_keys=True, indent=4)
        print '\n'.join([l.rstrip() for l in  s.splitlines()])


