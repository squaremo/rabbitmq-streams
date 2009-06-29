"""
Driver for Python plugins.  This will be invoked with a module path
and name and a pointer to the initialisation document (see [TODO]).
"""

try:
    import simplejson as json
except:
    import json
from threading import Thread
import sys, os.path
from imp import find_module, load_module
import threading

def main():
    sys.stdout.write(sys.argv[1] + '\n')
    sys.stdout.flush()
    args = json.loads(sys.stdin.readline())
    
    here = os.path.dirname(os.path.abspath(sys.argv[0]))

    plugin_type = args["plugin_name"]
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
        f.close()
        f = None
        if 'run' not in dir(module):
            raise "Module %r does not contain a run procedure" % module
        
        waiter = StdInWatcher()
        waiter.setDaemon(True)
        waiter.start()
        
        module.run(args)
    finally:
        if f is not None and not f.closed:
            f.close()
    # let main thread exit. All other threads are daemon threads, so
    # anyone calling sys.exit() causes python to exit

class StdInWatcher(Thread):
    def run(self):
        while not '' == sys.stdin.readline():
            pass
        sys.exit()

# This is not intended to be used as a module, but
# we follow Python idiom anyway.
if __name__ == '__main__':
    main()
