from __future__ import with_statement
import atexit
from functools import partial
import httplib
from optparse import OptionParser, make_option as opt
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

from feedshub import json

BAD_SYSTEM_STATE, BAD_CONFIG, BAD_CHANNEL, MALFORMED_INPUT = (2**n for n in range(4))
SEP="\t"
#IO_LINE_REX=re.compile(r'^([<>]?)|\.{3})?(\w*)\s*'+SEP+'(.*\n?)')
IO_LINE_REX=re.compile(r'^(?:\.{3}|([<>]?)([^'+SEP+']*?))\s*'+SEP+'(.*\n?)')
# TODO(alexander): remove hardwired config
RABBIT_CONNECTION_PARAMS = dict(host='localhost:5672', userid='feedshub_admin',
                                password='feedshub_admin', virtual_host='/')
COUCH_HOST_PORT = ("localhost", 5984)
HARNESS_BASE_DIR = os.path.join(here, '..', 'harness')

def info(*args):
    for arg in args:
        print arg,
    print


def json_repr(py_obj):
    # replace None w/ 0 to get indentation
    return json.dumps(py_obj, indent=None).replace('\n', '\n' + SEP)

def newname():
    return 'test/' + hashlib.sha1(os.urandom(8)).hexdigest()

def format_output(channel_name, msg):
    line_sep = "\n%s%s" % (' ' * len(channel_name), SEP)
    print ">%s%s%s" % (
        channel_name, SEP, line_sep.join(msg.body.split('\n')+[]))
    sys.stdout.flush()


class StdoutOutputter(object):
    def __init__(self, channel_names):
        self.channel_names = channel_names

    def make_output_callback(self, channel_name):
        assert channel_name in self.channel_names
        return partial(format_output, channel_name)

    def flush(self): pass

class BatchedOutputter(object):

    def __init__(self, channel_names, format_output=format_output):
        self.channel_names = channel_names
        self.outputs = {}
        self.format_output = format_output

    def make_output_callback(self, channel_name):
        assert channel_name in self.channel_names
        res = self.outputs[channel_name] = []
        def output_concer(msg):
            res.append(msg)
        return output_concer

    def flush(self):
        for c in sorted(self.outputs):
            ## print '#DEBUG c:', c
            while self.outputs[c]:
                ## print '#DEBUG o[c]:', self.outputs[c]
                self.format_output(c, self.outputs[c].pop(0))
        ## print '#DEBUG flushed!', self.outputs

def continously(f):
    class RepetitiveThread(threading.Thread):
        def run(self):
            while True: f()
    t = RepetitiveThread()
    t.daemon = True
    t.start()
    return t


class TestWiring(object):

    def __init__(self, amqp_connection, inputspec, outputspec,
                 Outputter=StdoutOutputter):
        self.channel = amqp_connection.channel()
        self.outputs = dict((spec['name'], self.declexchange()) for spec in outputspec)
        self.inputs = dict((spec['name'], self.declqueue()) for spec in inputspec)
        self.outputter = Outputter([spec['name'] for spec in outputspec])

        # Now, we want to *listen* to the outputs, and *talk* to the inputs
        for (name, exchange) in self.outputs.items():
            self.subscribe(name, exchange, )
        self.subscribe(name="log", exchange="feedshub/log", key='#',
                       mk_callback=partial(format_output, ">log"))

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

    def subscribe(self, name, exchange, key='', mk_callback=None):
        queue = self.declqueue()
        info("# %s bound to %s" % (name, queue))
        self.channel.queue_bind(queue, exchange, routing_key=key)
        self.channel.basic_consume(
            queue, no_ack=True,
            callback=mk_callback or self.outputter.make_output_callback(name))

    def make_talker(self, queue):
        exchange = self.declexchange()
        self.channel.queue_bind(queue, exchange)
        def say(something):
            self.channel.basic_publish(amqp.Message(body=something), exchange)
        return say

    def send(self, **kwargs):
        assert set(kwargs) == set(self.inputs) # XXX
        for in_name, to_say in kwargs.items():
            if isinstance(to_say, basestring):
                to_say = [to_say]
            for blahblah in to_say:
                self.talkers[in_name](blahblah)
        self.outputter.flush()




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

def setup_everything(plugindir, config_json, Outputter):
    instance_config = json.loads(config_json)

    info("<$PluginInstanceConfiguration%s%s" % (SEP, json_repr(instance_config)))
    info()

    with open(os.path.join(plugindir, 'plugin.js')) as f:
        plugin_specs = json.loads(f.read())

    info("## Plugin descriptor:")
    info("#", json_repr(plugin_specs))

    wiring = TestWiring(amqp_connection=amqp.Connection(**RABBIT_CONNECTION_PARAMS),
                        inputspec=plugin_specs['inputs_specification'],
                        outputspec=plugin_specs['outputs_specification'],
                        Outputter=Outputter)

    init_couch_state()

    def reconfigure_plugin(*args, **kwargs):
        assert 0 <= len(args) <= 1
        if args:
            assert not kwargs
            config, = args
            if isinstance(config, basestring):
                config = json.loads(config)
        else:
            config = instance_config.copy()
            config.update(kwargs)
        spawn_plugin(plugindir=plugindir, plugin_specs=plugin_specs,
                     instance_config=config,
                     inputs=wiring.inputs, outputs=wiring.outputs,
                     rabbit_params=RABBIT_CONNECTION_PARAMS)

    reconfigure_plugin()

    info()
    info("""##The input channels are: %s
    # type 'INPUT<tab>MESSAGE' to inject a message into channel INPUT,
    # type 'more-stuff' to continue the message and
    # type '' (an empty line) to finish it (all newlines but the last are kept).
    # type ^D to abort. You are free to insert whitespace before the ':'.
    ---
    """ % ' '.join(wiring.inputs))
    return wiring, reconfigure_plugin, instance_config



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
    info("## Initialising plugin with:")
    info("#", json.dumps(init))
    pluginproc.stdin.write(json.dumps(init) + "\n")
    # make sure we don't exit & leave this around!
    # TODO(alexander): haven't found a clean way that won't deadlock
    # python2.6 has a Popen.kill but for 2.5 we need this:
    atexit.register(lambda p=pluginproc.pid: os.kill(p, signal.SIGTERM))

    info("## Initialising plugin with:")
    return pluginproc


# TODO: a proper state-machine abstraction might be good here...
def repl(lines, send):
    WANT_CHANNEL, WANT_ANY = 'want_channel', 'want_any'

    talker_name, msg, state = None, None, WANT_CHANNEL
    exit_code = 0

    def ship():
        try:
            to_say = msg.rsplit('\n',1)[0]
            ## print >>sys.stderr, "A'shippin': %r"% to_say
            send(**{talker_name :to_say})
            ## print >>sys.stderr, "A'shipped"
            return 0
        except KeyError:
            print >>sys.stderr, ("ERROR(BAD_CHANNEL)%s%r unknown" %
                                 (SEP, talker_name))
            return BAD_CHANNEL

    for line in lines:
        ## print >>sys.stderr, "A'line read"
        if line.startswith('#'):
            pass
        elif not line.strip():
            if state == WANT_ANY:
                exit_code |= ship()
                talker_name, msg, state = None, None, WANT_CHANNEL
        else:
            try:
                typ, channel, bit = IO_LINE_REX.match(line).groups()
            except Exception:
                print >>sys.stderr, (
                    ">ERROR(MALFORMED_INPUT)%s"
                    " Bad input! (neither comment nor message):%r" % (SEP, line))
                continue
            if not channel:
                if state is WANT_CHANNEL:
                    print >>sys.stderr, (
                        ">ERROR(MALFORMED_INPUT)%s"
                        " need some channel to send message to!" % (SEP,))
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
                        ">ERROR(MALFORMED_INPUT)%s"
                        "%r on its own is not meaningful" % (SEP, bit))
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
        ## print >> sys.stderr, 'EOL'
    if talker_name is not None:
        exit_code |= ship()
    return exit_code


def update(dict, stuff):
    if ininstance(stuff, basestring):
        stuff = json_decode_error

false, true, null = False, True, None  # make json valid python

if __name__ == '__main__':
    opts, args = OptionParser("""%prog [OPTIONS] PLUGINPATH [PLUGIN_CONFIG]

    Test a plugin by sending input and observing its output.
    """,
                              [opt(None, "--py", action="store_true",
                                   help="If true, leave control to python, else start I/O repl")
                               ]).parse_args()
    if not args or len(args) > 2:
        print >> sys.stderr, "Illegal number of arguments (%d)" % len(args), "try",\
              sys.argv[0], "--help"
        sys.exit(os.EX_USAGE)
    print args, opts.py
    setup_args = dict(plugindir  = os.path.abspath(args.pop(0)),
                      config_json = open(args.pop(0)).read() if args else '{}',
                      Outputter = BatchedOutputter if opts.py else StdoutOutputter)

    wiring, reconfigure_plugin, config = setup_everything(**setup_args)
    send = wiring.send

    if not opts.py:
        sys.exit(repl(iter(sys.stdin.readline, ''), wiring.send))

    ## import code
    ## code.interact('', lambda x: sys.stdin.readline().replace('\n',''), vars())

    ## if opts.py:
    ##     from IPython.Shell import IPShellEmbed
    ##     ipshell = IPShellEmbed(args,
    ##                            banner = 'Dropping into IPython',
    ##                            exit_msg = 'Leaving Interpreter, back to program.')




