import sys, os.path, string
path = os.path.dirname(sys.argv[0])
if len(path) > 0:
    path = path + "/"
path = path + "../harness/python/lib"
sys.path.append(path)

try:
    import json
except ImportError:
    import simplejson as json

import couchdb.client as couch

args = sys.argv[1:]

if len(args) == 0:
    print 'Usage list_feeds.py [feeds|terminals|servers] ?active ?dump id*\n'
    sys.exit(0)

entry_type = 'feed'

view = '_design/feeds/_view/join'
if len(args) > 0:
    view_arg = string.lower(args[0])
    if view_arg == 'feeds':
        view = '_design/feeds/_view/join'
        args = args[1:]
        entry_type = 'feed'
    elif view_arg == 'servers':
        view = '_design/servers/_view/join'
        args = args[1:]
        entry_type = 'server'
    elif view_arg == 'terminals':
        view = '_design/terminals/_view/join'
        args = args[1:]
        entry_type = 'terminal'

entry_status = entry_type + '-status'

if len(args) > 0 and string.lower(args[0]) == "active":
    view = view + "_active"
    args = args[1:]

dump = False
if 'dump' == args[0]:
    dump = True
    args = args[1:]

db = couch.Database('http://localhost:5984/feedshub_status/')
for row in db.view(view, group=True):
    if row.value != None:
        doc_id = row.value[entry_type]['_id']
        if len(args) > 0 and not doc_id in args:
            continue
        if dump:
            s = json.dumps(row.value, sort_keys=True, indent=4)
            print '\n'.join([l.rstrip() for l in  s.splitlines()])
        else:
            active = 'inactive'
            if row.value[entry_status]['active']:
                active = 'active'
            print row.value[entry_type]['_id'] + '\t' + active


