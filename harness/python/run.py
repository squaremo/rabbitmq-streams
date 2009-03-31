"""
Driver for Python plugins.  This will be invoked with a module path
and name and a pointer to the initialisation document (see [TODO]).
"""

try:
    import json
except:
    import simplejson as json

def main():
    import sys, os.path
    from imp import find_module, load_module
    args = json.loads(sys.stdin.readline())
    
    here = os.path.dirname(os.path.abspath(sys.argv[0]))

    plugin_type = args["config"]["type"]
    plugin_dir = args["plugin_dir"]
    
    #plugin_path = os.path.join(plugin_dir, plugin_type)

    # It would be nice to do this, but it doesn't seem to be allowed.
    #os.chroot(plugin_dir)
    
    sys.path.insert(0, plugin_dir)
    sys.path.insert(0, os.path.join(plugin_dir, 'lib'))
    sys.path.insert(0, here)
    sys.path.insert(0, os.path.join(here, 'lib'))
    f = None
    os.chdir(plugin_dir)
    try:
        f, p, d = find_module(plugin_type, [plugin_dir])
        module = load_module(plugin_type, f, p, d)
        if 'run' not in dir(module):
            raise "Module %r does not contain a run procedure" % module
        print json.dumps({"status": "ok"})
        module.run(args)
    finally:
        if f is not None:
            f.close()

# This is not intended to be used as a module, but
# we follow Python idiom anyway.
if __name__ == '__main__':
    main()
