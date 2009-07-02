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
import thread, time, os

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
    # Let main thread exit. All other threads should be daemon
    # threads, so the VM should exit at this point.

class StdInWatcher(Thread):
    def run(self):
        while not '' == sys.stdin.readline():
            pass
        thread.interrupt_main()
        # BUG: When we have an open amqplib AMQP connection, the
        # KeyboardInterrupt generated by the previous line seems to
        # vanish completely, never being raised in any thread's
        # context. It's a complete mystery. I've spent a long time
        # trying to figure out what's going on -- until we work this
        # out, a brutal kill after a short wait seems like the
        # appropriate thing to do. If anyone discovers the cause of
        # the missing KeyboardInterrupt, please let me know! --
        # tonyg@lshift.net, 29 June 2009
        time.sleep(0.2)
        os._exit(0)

# This is not intended to be used as a module, but
# we follow Python idiom anyway.
if __name__ == '__main__':
    main()
