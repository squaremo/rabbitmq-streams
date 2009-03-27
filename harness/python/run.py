"""
Driver for Python plugins.  This will be invoked with a module path
and name and a pointer to the initialisation document (see [TODO]).
"""

try:
    import json
except:
    import simplejson as json

def usage():
    print """
    python run.py <modulepath> <modulename> <URL>

    modulepath is the path to the plugin
    modulename is the name of a module on the path that is to be treated as a component.
    URL is the URL of an initialisation document.

    The plugin will be given an effective PYTHONPATH of at least the
    harness directory, its own directory and the directory 'lib/'
    under that.
    """

def fetch_config(url):
    """
    I get the configuration as pointed to by the initialisation
    document.  If there's a convention as to how configuration is laid
    out (for example, if it's a set of pointers to other documents),
    this is where that convention will be encoded.
    """
    import urllib2
    try:
        doc = urllib2.urlopen(url)
    except Exception, e:
        raise "Error opening URL for initialisation: %s" % url, e

    try:
        config = json.loads(doc.read())
    except Exception, e:
        raise "Error reading initialisation document at %s" % url, e
    return config


def main():
    import sys, os.path
    from imp import find_module, load_module
    if len(sys.argv) < 4:
        return usage()
    here = os.path.dirname(os.path.abspath(sys.argv[0]))
    modulepath = sys.argv[1]
    modulename = sys.argv[2]
    url = sys.argv[3]
    config = fetch_config(url)

    # It would be nice to do this, but it doesn't seem to be allowed.
    #os.chroot(modulepath)
    
    sys.path.insert(0, modulepath)
    sys.path.insert(0, os.path.join(modulepath, 'lib'))
    sys.path.insert(0, os.path.join(here, 'lib'))
    f = None
    try:
        f, p, d = find_module(modulename, [modulepath])
        module = load_module(modulename, f, p, d)
        if 'run' not in dir(module):
            raise "Module %r does not contain a run procedure" % module
        module.run(config)
    finally:
        if f is not None:
            f.close()

# This is not intended to be used as a module, but
# we follow Python idiom anyway.
if __name__ == '__main__':
    main()
