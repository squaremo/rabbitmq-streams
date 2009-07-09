from __future__ import with_statement
import atexit
import httplib
import os
import posixpath
import re
import hashlib
import signal
import subprocess
import sys
import threading

here = os.path.dirname(os.path.abspath(sys.argv[0]))
sys.path.insert(0, os.path.join(here, '../harness/python'))
sys.path.insert(0, os.path.join(here, '../harness/python/lib'))

import amqplib.client_0_8 as amqp

from feedshub import json, plugin_config_header

BAD_SYSTEM_STATE, BAD_CONFIG, BAD_CHANNEL, MALFORMED_INPUT = (2**n for n in range(4))
#                         |1 i/o|2 ch|  4 rk  |   6 config       |7 data|
IO_LINE_REX=re.compile(r'^([<>]?)(\w*)(/(\w*))?(\(([^\)]*)\))?\s*:(.*\n?)') # TODO(alexander): cut'n pasted
#talkerRe = re.compile("([a-zA-Z0-9]*)(/([a-zA-Z0-9]*))?(\(([^\)]*)\))?:(.*)")

# TODO(alexander): remove hardwired config
RABBIT_CONNECTION_PARAMS = dict(host='localhost:5672', userid='feedshub_admin',
                                password='feedshub_admin', virtual_host='/')
COUCH_HOST_PORT = ("localhost", 5984)
HARNESS_BASE_DIR = os.path.join(here, '..', 'harness')

def json_repr(py_obj):
    # replace None w/ 0 to get indentation
    return json.dumps(py_obj, indent=None).replace('\n', '\n\t:')

def newname():
    return 'test-%s' % hashlib.sha1(os.urandom(8)).hexdigest()

def make_stdout_msg_outputter(name):
    def stdout_msg_outputter(msg):
        line_sep = "\n%s :" % (' ' * len(name))
        print (">%s:%s" % (name, line_sep.join(msg.body.split('\n')+[])))
    return stdout_msg_outputter

def continously(f):
    class RepetitiveThread(threading.Thread):
        def run(self):
            while True: f()
    t = RepetitiveThread()
    t.daemon = True
    t.start()
    return t

class TestWiring(object):

    def __init__(self, amqp_connection, inputspec, outputspec):
        self.channel = amqp_connection.channel()
        self.outputs = dict((spec['name'], self.declexchange()) for spec in outputspec)
        self.inputs = dict((spec['name'], self.declqueue()) for spec in inputspec)

        # Now, we want to *listen* to the outputs, and *talk* to the inputs
        for (name, exchange) in self.outputs.items():
            self.subscribe(name, exchange)
        self.subscribe("log", "feedshub/log", '#')

        self.talkers = dict((name, self.make_talker(queue))
                            for (name, queue) in self.inputs.items())

        self.listener = continously(self.channel.wait)

    def declexchange(self):
        name = newname()
        self.channel.exchange_declare(name, 'fanout')
        return name

    def declqueue(self):
        name = newname()
        self.channel.queue_declare(name)
        return name

    def info(self, msg):
        print msg

    def subscribe(self, name, exchange, key='', make_outputter=make_stdout_msg_outputter):
        queue = self.declqueue()
        self.info("# %s bound to %s" % (name, queue))
        self.channel.queue_bind(queue, exchange, routing_key=key)
        self.channel.basic_consume(queue, no_ack=True, callback=make_outputter(name))

    def make_talker(self, queue):
        exchange = self.declexchange()
        self.channel.queue_bind(queue, exchange)
        def say(something, rk='', config=None):
            if config is not None:
                headers = {plugin_config_header: config}
                self.channel.basic_publish(amqp.Message(body=something,
                                                   application_headers=headers),
                                      exchange,
                                      routing_key=rk)
            else:
                self.channel.basic_publish(amqp.Message(body=something),
                                      exchange,
                                      routing_key=rk)
        return say


def init_couch_state(host=COUCH_HOST_PORT[0], port=COUCH_HOST_PORT[1]):
    couch = httplib.HTTPConnection(host, port)
    #TODO(alexander): this is stupid, what it should really be doing is create
    #an unique id; unfortunately the dreaded document update conflict needs to
    #be resolved first
    for req, allowable in [(("DELETE", "/plugin_test_harness"), range(600)),
                           (("PUT", "/plugin_test_harness"), range(400))]:
        couch.request(*req)
        try:
            ans = couch.getresponse()
            ans_s = ans.read()
        except Exception, msg:
            print >> sys.stderr, (
                "Couldn't successfully talk to CouchDB: %s -> %s" % (req, msg))
            sys.exit(BAD_SYSTEM_STATE)
        if ans.status not in allowable:
            print >> sys.stderr, (
                "Could successfully talk to CouchDB: %s -> %3d: %s" % (
                req, ans.status, ans_s))
            sys.exit(BAD_SYSTEM_STATE)

def main(plugindir, config_json):
    instance_config = json.loads(config_json)

    print "<$PluginInstanceConfiguration: ", json_repr(instance_config)
    print

    with open(os.path.join(plugindir, 'plugin.js')) as f:
        plugin_specs = json.loads(f.read())

    print "## Plugin descriptor:"
    print "#", json_repr(plugin_specs)

    wiring = TestWiring(amqp_connection=amqp.Connection(**RABBIT_CONNECTION_PARAMS),
                        inputspec=plugin_specs['inputs_specification'],
                        outputspec=plugin_specs['outputs_specification'])

    init_couch_state()

    spawn_plugin(plugindir=plugindir, plugin_specs=plugin_specs, instance_config=instance_config,
                 inputs=wiring.inputs, outputs=wiring.outputs,
                 rabbit_params=RABBIT_CONNECTION_PARAMS)

    print
    print """##The input channels are: %s
    # type 'INPUT:message' to inject a message into channel INPUT,
    # type ':more-stuff' to continue the message and
    # type '' (an empty line) to finish it (all newlines but the last are kept).
    # type ^D to abort. You are free to insert whitespace before the ':'.
    ---
    """ % ' '.join(wiring.inputs)

    sys.exit(repl(iter(sys.stdin.readline, ''), wiring.talkers))


def spawn_plugin(plugindir, plugin_specs, instance_config,
                 inputs, outputs,
                 rabbit_params=RABBIT_CONNECTION_PARAMS,
                 ):
    plugin_test_doc_url = "http://%s:%d/plugin_test_harness" % tuple(COUCH_HOST_PORT)
    statedocname = newname()
    # Assemble the plugin init
    init = {
        "harness_type": plugin_specs['harness'],
        "plugin_name": os.path.basename(plugindir),
        "plugin_dir": plugindir,
        "feed_id": "test",
        "node_id": "plugin",
        "plugin_type": plugin_specs, # TODO(alexander): this looks *wrong*
        "global_configuration": {}, # TODO(alexander): excise at some point
        "configuration": instance_config, # this comes from the feeds config, the node in the wiring
        # TODO(alexander): is this inconsistent naming necessary?
        "messageserver":  {'host' : rabbit_params['host'].split(':')[0],
                           'port' : rabbit_params['host'].split(':')[1],
                           'username' : rabbit_params['userid'],
                           'password' : rabbit_params['password'],
                           'virtual_host' : rabbit_params['virtual_host']
                           },
        "inputs":  inputs, # Q name provided by orchestrator
        "outputs": outputs, # Exchange name provided by orchestrator
        "state":  posixpath.join(plugin_test_doc_url, statedocname),
        "database": plugin_test_doc_url
    }

    harnessdir = os.path.join(HARNESS_BASE_DIR, init['harness_type'])
    harness = os.path.join(harnessdir, 'run_plugin.sh')
    pluginproc = subprocess.Popen([harness], cwd=harnessdir,
                                  stderr=subprocess.STDOUT, stdin=subprocess.PIPE)
    print "## Initialising plugin with:"
    print "#", json.dumps(init)
    pluginproc.stdin.write(json.dumps(init) + "\n")
    # make sure we don't exit & leave this around!
    # TODO(alexander): haven't found a clean way that won't deadlock
    # python2.6 has a Popen.kill but for 2.5 we need this:
    atexit.register(lambda p=pluginproc.pid: os.kill(p, signal.SIGTERM))

    print "## Initialising plugin with:"
    return pluginproc

def parseInput(line):
    m = IO_LINE_REX.match(line)
    if m is None:
        raise "Does not match"
    else:
        (io, channel, _1, rk, _2, conf, msg) = m.groups()
        c = None
        if conf is not None:
            # let the exception bubble up
            c = json.loads(conf)
        return (io, channel, rk or '', c, msg)
            
# TODO: a proper state-machine abstraction might be good here...
def repl(lines, talkers):
    WANT_CHANNEL, WANT_ANY = 'want_channel', 'want_any'

    talker_name, msg, rk, conf, state = None, None, "", None, WANT_CHANNEL
    exit_code = 0

    def ship():
        try:
            to_say = msg.rsplit('\n',1)[0]
            ## print >>sys.stderr, "A'shippin': %r"% to_say
            talkers[talker_name](to_say, rk=rk, config=config)
            return 0
        except KeyError:
            print >>sys.stderr, ("ERROR(BAD_CHANNEL): %r unknown" %
                                 talker_name)
            return BAD_CHANNEL

    for line in lines:
        if line.startswith('#'):
            pass
        elif not line.strip():
            if state == WANT_ANY:
                exit_code |= ship()
                talker_name, msg, state = None, None, WANT_CHANNEL
        else:
            try:
                typ, channel, rk, config, bit = parseInput(line)
            except Exception:
                print >>sys.stderr, (
                    ">ERROR(MALFORMED_INPUT):"
                    " Bad input! (neither comment nor message):%r" % line)
                continue
            if not channel:
                if state is WANT_CHANNEL:
                    print >>sys.stderr, (
                        ">ERROR(MALFORMED_INPUT):"
                        " need some channel to send message to!")
                    exit_code |= MALFORMED_INPUT
                    continue
                elif not bit and not typ:
                    exit_code |= ship()
                    talker_name, msg, state = None, None, WANT_CHANNEL
                elif not typ:
                    msg += bit
                    state = WANT_ANY
                else:
                    print >>sys.stderr, (
                        ">ERROR(MALFORMED_INPUT):"
                        "%r on its own is not meaningful" % bit)
                    exit_code |= MALFORMED_INPUT
                    continue
            else:
                if typ != '>':
                    if state is WANT_ANY:
                        exit_code |= ship()
                    talker_name = channel
                else:
                    talker_name = None # FIXME
                msg = bit
                state = WANT_ANY
    if talker_name is not None:
        exit_code |= ship()
    return exit_code

if __name__ == '__main__':
    pass
    main(plugindir=os.path.abspath(sys.argv[1]),
         config_json=(len(sys.argv) > 2) and open(sys.argv[2]).read() or "{}")
