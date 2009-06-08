
import feedparser, time, sha, types
import web.template as template

rss = template.frender('atom.template')

def autoproperty(propname):
    return property(lambda self: getattr(self, propname),
                    lambda self, val: setattr(self, propname, val))

def shahex(s):
    return sha.new(s).hexdigest()

def restrict(d, keys):
    res = {}
    for k in keys:
        if k in d:
            res[k] = d[k]

    return res

entry_fields = [ 'title', 'title_detail', 'link', 'links',
                 'subtitle', 'subtitle_detail', 'rights', 'rights_detail',
                 'id', 'author', 'author_detail', 'content' ]

status_fields = ['status', 'href', 'etags', 'modified']

def stable_repr(val):
    """Like repr but with a guranteed fixed ordering ('<' actually) for (sub-)dicts.

    Note: handles only the basic builtin python container types.

    >>> stable_repr([(dict((k, dict(enumerate("abcd"))) for k in "ab"),)])
    [({'a': {0: 'a', 1: 'b', 2: 'c', 3: 'd'}, 'b': {0: 'a', 1: 'b', 2: 'c', 3: 'd'}})]

    """
    t = type(val)
    if t == types.ListType:
        return '[' + ', '.join(map(stable_repr, val)) + ']'
    elif t == types.TupleType:
        return '(' + ', '.join(map(stable_repr, val)) + ')'
    elif t == types.DictType:
        return '{' + ', '.join(sorted([stable_repr(x) + ': ' + stable_repr(y)
                                       for (x,y) in val.iteritems()])) + '}'
    else:
        return repr(val)

def dbindex(db):
    return set(db) # works because DB implements __iter__

def feed_status(feed):
    status = restrict(feed, status_fields)
    if status.has_key('modified'):
        status['modified'] = time.mktime(status['modified']) # seconds easier to deal with
    return status

def fetch(database, href, feed_args):
    feed = feedparser.parse(href, **feed_args)
    
    now = time.time()
    if feed.bozo:
        e = feed.bozo_exception
        # FIXME: this could well break for other kinds of exception
        if (hasattr(e, 'getMessage') and hasattr(e, 'getLineNumber')):
            return dict(error=dict(message = e.getMessage(),
                                   line = e.getLineNumber()),
                        lastpolled=now,
                        feed=feed_status(feed))
        else:
            return dict(error=dict(message = repr(e)),
                        lastpolled = now,
                        feed=feed_status(feed))
    
    def docid(e):
        id = e.get('id')
        if id:
            return shahex(id)
        else:
            return shahex(stable_repr(e))
        
    def doc(e):
        e = restrict(e, entry_fields)
        return dict(_id=docid(e), entry=unicode(rss(entry=e)))

    index = dbindex(database)
    # TODO: do this in two steps, one to get IDs, one to use template
    docs = [d for d in map(doc, feed.entries) if d['_id'] not in index]
    # TODO: updated entries
    return dict(lastpolled=now,
                feed=feed_status(feed),
                updated=list(docs))
