# -*- encoding: utf-8 -*-
from __future__ import with_statement
import atexit
from functools import partial
import hashlib
import httplib
from optparse import OptionParser, OptionGroup, make_option as opt
import os
import posixpath
import re
import time
import signal
import subprocess
import sys
import threading

here = os.path.dirname(os.path.abspath(sys.argv[0]))
sys.path.insert(0, os.path.join(here, '../harness/python'))
sys.path.insert(0, os.path.join(here, '../harness/python/lib'))

import amqplib.client_0_8 as amqp

from feedshub import json, plugin_config_header as PLUGIN_CONFIG_HEADER

BAD_SYSTEM_STATE, BAD_CONFIG, BAD_CHANNEL, MALFORMED_INPUT = (2**n for n in range(4))
SEP, IN, OUT = "\t><"
CONT = "..."

MODS = frozenset('sleep rk config headers'.split())

class ReplError(Exception): pass


# TODO(alexander): remove hardwired config
RABBIT_CONNECTION_PARAMS = dict(host='localhost:5672', userid='feedshub_admin',
                                password='feedshub_admin', virtual_host='/')
COUCH_HOST_PORT = ("localhost", 5984)
HARNESS_BASE_DIR = os.path.join(here, '..', 'harness')

__processes_to_kill = []
def waste():
    global __processes_to_kill
    while __processes_to_kill:
        os.kill(__processes_to_kill.pop().pid, signal.SIGTERM)
atexit.register(waste)


verbosity = 1
def info(*args):
    global verbosity
    just_comments = ((args or "#")[0].startswith('#'))
    if verbosity > just_comments:
        for arg in args:
            print arg,
        print
        sys.stdout.flush()


def json_repr(py_obj):
    # replace None w/ 0 to get indentation
    return json.dumps(py_obj, indent=None).replace('\n', '\n' + SEP)

def newname():
    return 'test-' + hashlib.sha1(os.urandom(8)).hexdigest()

def format_output(channel_name, msg):
    line_sep = "\n%s%s%s" % (CONT, ' ' * (len(OUT)+len(channel_name)), SEP)
    print "%s%s%s%s" % (
        OUT, channel_name, SEP, line_sep.join(msg.body.split('\n')+[]))
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
        def output_concer(msg):
            self.outputs.setdefault(channel_name, []).append(msg)
        return output_concer

    def flush(self):
        for c in sorted(self.outputs):
            while self.outputs[c]:
                self.format_output(c, self.outputs[c].pop(0))
        self.outputs.clear()


class Failure(object):
    def __init__(self, expected, obtained):
        self.expected, self.obtained = expected, obtained
    def __repr__(self):
        return "Failure(expected=%r,\n        obtained=%r)\n" % (self.expected, self.obtained)

class Success(object):
    def __init__(self, obtained):
        self.obtained = obtained
    def __repr__(self):
        return "Success(obtained=%r)\n" % (self.obtained,)


class TestOutputter(object):

    def __init__(self, channel_names, format_output=None):
        self.channel_names = channel_names
        self.outputs = [{}]
        self.results = []
        self.attempted = 0
        self.failed = 0

    def make_output_callback(self, channel_name):
        assert channel_name in self.channel_names
        def output_concer(msg):
            self.outputs[-1].setdefault(str(channel_name), []).append(Msg(msg.body))
        return output_concer

    def expect(self, **kwargs):
        assert not (set(kwargs) - set(self.channel_names))
        expected, obtained = kwargs, self.outputs[-1]
        self.attempted += 1
        ## print >>sys.stderr, "###DEBUG", expected, obtained
        ok = expected == obtained
        if ok:
            self.results.append(Success(obtained=obtained))
        else:
            self.results.append(Failure(expected=expected, obtained=obtained))
            self.failed += 1
        self.outputs.append({})

    def flush(self): pass

    def __str__(self):
        filename = sys.stdin.name # FIXME(alexander): horrible HACK
        ans = []
        for r in self.results:
            if isinstance(r, Failure):
                first_lineno = min(getattr(msg, 'lineno', sys.maxint)
                                   for msgs in r.expected.values()
                                   for msg in msgs)
                if sorted(r.expected.keys()) != sorted(r.obtained.keys()):
                    expectations, obtainments = r.expected, r.obtained
                    ans.append("%s:%s: Test Failure:\nexpected:%s\nobtained:%s\n" % (
                        filename, first_lineno, r.expected, r.obtained))
                else:
                    for k in r.expected.keys():
                        if r.expected[k] != r.obtained[k]:
                            ans.append("%s:%s: Test Failure:\nexpected:%s\nobtained:%s\n" %
                                       (filename, getattr((r.expected[k] or [None])[0],
                                                          'lineno', first_lineno),
                                        r.expected[k], r.obtained[k]))
        return "%sTotals for %s:  failed=%d, attempted=%d" % (
            "\n".join(ans+[]), filename, self.failed, self.attempted)



def continously(f):
    class RepetitiveThread(threading.Thread):
        must_die = False
        def run(self):
            while True:
                if self.must_die: break
                f()
    t = RepetitiveThread()
    t.daemon = True
    t.start()
    def pull_the_plug():
        t.must_die = 1
    return pull_the_plug

def secondify(string):
    r"""Interpret a string as a number of seconds.

    >>> map(secondify, ['60', '6e1s', '60.0e0 s', '1.0 min', '6e7us'])
    [60.0, 60.0, 60.0, 60.0, 60.0]
    >>> secondify('0.5')
    0.5
    >>> secondify('foobar') #doctest: +ELLIPSIS
    Traceback (most recent call last):
    ...
    ValueError: ...
    """
    INTERVALS = dict(us=1e-6,ms=1e-3,s=1, min=60, h=60*60)
    m = re.match(r"(.*?)\s*([um]?s|min|h|d)?$", str(string.replace(u'μ','u')))
    if not m: raise ValueError("Not a valid time duration string: %r" % string)
    else:     return float(m.group(1)) * INTERVALS[m.group(2) or 's']




class TestWiring(object):

    def __init__(self, amqp_connection, inputspec, outputspec,
                 Outputter=StdoutOutputter):
        self.args = amqp_connection, inputspec, outputspec, Outputter

    def reset(self):
        amqp_connection, inputspec, outputspec, Outputter = self.args
        self.channel = amqp_connection.channel()
        self.outputs = dict((spec['name'], self.declexchange()) for spec in outputspec)
        self.outputs['notify'] = 'feedshub/notify' # XXX
        self.outputs['log'] = 'feedshub/log' # XXX
        self.inputs = dict((spec['name'], self.declqueue()) for spec in inputspec)
        self.outputter = Outputter(self.outputs.keys())

        global verbosity
        # Now, we want to *listen* to the outputs, and *talk* to the inputs
        for (name, exchange) in self.outputs.items():
            if name not in ('notify', 'log'):
                self.subscribe(name, exchange)
            else:
                if name == 'notify' or verbosity > 1:
                    self.subscribe(name=name, exchange=exchange, key='#')



        self.talkers = dict((name, self.make_talker(queue))
                            for (name, queue) in self.inputs.items())

        self.kill_listener = continously(self.channel.wait)

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
        def say(body, rk='', config=None):
            if config is not None:
                headers = {PLUGIN_CONFIG_HEADER: config}
                self.channel.basic_publish(
                    amqp.Message(body=body, application_headers=headers),
                    exchange,
                    routing_key=rk)
            else:
                self.channel.basic_publish(amqp.Message(body=body),
                                      exchange,
                                      routing_key=rk)
        return say

    def _normalize_kwargs(self, kwargs):
        res = {}
        for in_name, to_say in kwargs.items():
            if isinstance(to_say, basestring):
                to_say = [Msg(body=to_say)]
            if isinstance(to_say, Msg):
                to_say = [to_say]
            for cant in to_say:
                res[in_name] =  to_say
        return res

    def send(self, **kwargs):
        """Usage (in order of convenience forms -> general):
              send(channel_a="text", channel_b="more text")
              send(channel=["1st message", "2nd msg"], ...)
              send(channel_a=[Msg("text", rk="nsa", config={"FYI":True}), ...], ...)
        """
        if 'SLEEP' in kwargs:
            sleeps = kwargs.pop('SLEEP')
            if isinstance(sleeps, basestring): sleeps = [sleeps]
            elif isinstance(sleeps, (int, float)): sleeps = [repr(sleeps)]
            for s in sleeps: time.sleep(secondify(s))
        assert not (set(kwargs) - set(self.inputs))
        kwargs = self._normalize_kwargs(kwargs)
        for in_name, to_say in kwargs.items():
            for cant in to_say:
                self.talkers[in_name](cant.body, rk=cant.rk, config=cant.config)
        time.sleep(1) # FIXME HORRIBLE HACK
        self.outputter.flush()

    def teardown(self):
        self.kill_listener()
        self.talkers.popitem()[1]("BYEBYE")
        for name in self.outputs.values(): self.channel.exchange_delete(name)
        for name in self.inputs.values(): self.channel.queue_delete(name)
    __del__ = teardown


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

def is_server(spec):
    return spec['subtype']=='server'

def setup_everything(plugindir, config_json, env, Outputter):
    try:
        instance_config = json.loads(config_json)
    except ValueError:
        print >> sys.stderr, OUT+"ERROR\tExpected a valid json dictionary, got: %r" % (config_json,)
        sys.exit(255)

    info(IN + "PLUGIN_INSTANCE_CONFIG%s%s" % (SEP, json_repr(instance_config)))
    info("## Environment")
    info("# %r" % env)

    with open(os.path.join(plugindir, 'plugin.js')) as f:
        plugin_specs = json.loads(f.read())

    info("## Plugin descriptor:")
    info("#", json_repr(plugin_specs))
    inputspec = [{"name": "input"},
                 {"name": "command"}] if is_server(plugin_specs) \
                                      else plugin_specs['inputs_specification']
    outputspec = [{'name': 'output'}] if is_server(plugin_specs) \
                                      else plugin_specs['outputs_specification']
    wiring = TestWiring(amqp_connection=amqp.Connection(**RABBIT_CONNECTION_PARAMS),
                        inputspec=inputspec,
                        outputspec=outputspec,
                        Outputter=Outputter)

    init_couch_state()

    def reconfigure_plugin(*args, **kwargs):
        waste()
        wiring.reset()
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
                     instance_config=config, env=env,
                     inputs=wiring.inputs, outputs=wiring.outputs,
                     rabbit_params=RABBIT_CONNECTION_PARAMS)

    reconfigure_plugin()

    info("""##The input channels are: %s
# type 'INPUT<tab>MESSAGE' to inject a message into channel INPUT,
# type '<tab>more-stuff' to continue the message (all newlines but the last are kept)
# repeat as required if there are multiple input channels then
# type '' (an empty line) to finish the input group
# type ^D to exit.
""" % ' '.join(wiring.inputs))
    return wiring, reconfigure_plugin, instance_config



def spawn_plugin(plugindir, plugin_specs, instance_config, env,
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
    if (is_server(plugin_specs)):
        init['server_id'] = init['node_id']
        # TODO: make this a dummy terminal database
        init['terminals_database'] = "http://%s:%d/feedshub_status/" % tuple(COUCH_HOST_PORT)
    harnessdir = os.path.join(HARNESS_BASE_DIR, init['harness_type'])
    harness = os.path.join(harnessdir, 'run_plugin.sh')
    pluginproc = subprocess.Popen([harness], cwd=harnessdir,
                                  stdout=subprocess.PIPE, # TODO(alexander):
                                  env=env,
                                  stderr=subprocess.STDOUT, stdin=subprocess.PIPE)
    info("## Harness PID", pluginproc.stdout.readline())
    info("## Initialising plugin with:")
    info("#", json.dumps(init))
    pluginproc.stdin.write(json.dumps(init) + "\n")
    # make sure we don't exit & leave this around!
    # TODO(alexander): haven't found a clean way that won't deadlock
    # python2.6 has a Popen.kill but for 2.5 we need this:
    # whack all lingering plugin processes
    __processes_to_kill.append(pluginproc)
    return pluginproc





################################## Parsing ###################################

IO_LINE_REX=re.compile(r'''(?x)
 ^(?:                                                       # either
       (?:                                                    #     nothing
          |\.{3}                                              #  or '...'
          |(?P<typ>[<>]?)(?P<chan>\w+)(?P<json>[^%(SEP)s]*)   #  or [{'<','>'}] CHANNEL [JSON]
       )                                                      # followed by
       \s*%(SEP)s                                             #  SEP [MSG_PART]
       (?P<bit>.*?)
      )?                         # or nothing at all (empty line)
\n?$''' % dict(SEP=re.escape(SEP)))

strip_final_newline = partial(re.compile(r'\n\Z').sub, '')

def parse_io_line(line):
    r"""Parse a repl input line into chan_type, name, opts, msg_part.
    >>> parse_io_line(">input stuff")
    Traceback (most recent call last):
    ...
    ReplError: Unparsable input line(did you forget a '\t'?): '>input stuff'
    >>> parse_io_line("")
    (None, None, {}, None)
    >>> parse_io_line("\t")
    (None, None, {}, '')
    >>> parse_io_line("...\t")
    (None, None, {}, '')
    >>> parse_io_line("...\tmsg")
    (None, None, {}, 'msg')
    >>> parse_io_line("...\tmsg\n")
    (None, None, {}, 'msg')
    >>> parse_io_line("\tmsg")
    (None, None, {}, 'msg')
    >>> parse_io_line(IN + "ch\tmsg") == (IN, 'ch', {}, 'msg')
    True
    >>> parse_io_line("ch\tmsg") == (IN, 'ch', {}, 'msg')
    True
    >>> parse_io_line(OUT + "ch\tmsg") == (OUT, 'ch', {}, 'msg')
    True
    >>> parse_io_line(">ch\tmsg\n")
    ('>', 'ch', {}, 'msg')
    >>> parse_io_line(">ch\tmsg")
    ('>', 'ch', {}, 'msg')
    >>> parse_io_line('input{"sleep":3}\tmsg') ==  (IN, 'input', {u'sleep': 3}, 'msg')
    True
    >>> parse_io_line('input{"config":{"regexp"  : "foobar"}}\tmsg') == (IN, 'input', {u'config': {u'regexp': u'foobar'}}, 'msg')
    True
    >>> parse_io_line('input{"conf":{"regexp"  : "foobar"}}\tmsg')
    Traceback (most recent call last):
    [...]
    ReplError: Got the following unknown keys: conf (known keys are: headers,config,sleep,rk)
    """
    # [[sleep], rk, config, headers]
    try:
        d=IO_LINE_REX.match(line).groupdict()
    except AttributeError:
        raise ReplError("Unparsable input line%s: %r" % (
            ("" if SEP in line else ("(did you forget a %r?)" % SEP)), line))
    chan_type, chan, opts, msg_part = None, None, {}, d['bit']
    if d['chan']:
        chan_type = d['typ'] or IN
        chan = d['chan']
        if d['json']:
            try:
                opts = json.loads(d['json'])
                if not isinstance(opts, dict): raise ValueError
            except ValueError:
                raise ReplError("Expected a json dictionary, got: %s", d['json'])
            bad_keys = set(opts) - MODS
            if bad_keys:
                raise ReplError(
                    "Got the following unknown keys: %s (known keys are: %s)" % (
                        ",".join(bad_keys), ",".join(MODS)))
    return chan_type, chan, opts, msg_part

class Msg:
    def __init__(self, body, rk="", config=None):
        self.body, self.rk, self.config = body, rk, config
    def __repr__(self):
        return "Msg(body=%r, rk=%r, config=%r)" % (
            self.body, self.rk, self.config)
    def __eq__(self, other):
        return type(self) == type(other) and (
            (self.body, self.rk, self.config) == (other.body, other.rk, other.config))
    def __ne__(self, other): return not (self == other)

# TODO: a proper state-machine abstraction might be good here...
def repl(lines, send, expect=False, is_valid_channel=lambda *_: True, lineno=1):
    WANT_CHANNEL, WANT_ANY = 'want_channel', 'want_any'
    talker_name, msg, state = None, None, WANT_CHANNEL
    exit_code = 0
    io_group = {}
    channel_acc = []
    def ship():
        try:
            ## print >>sys.stderr, "###DEBUG A'shippin': %r"% io_group
            if group_chan_type == IN:
                send(**io_group)
            else:
                assert group_chan_type == OUT
                expect(**io_group)
            io_group.clear()
            ## print >>sys.stderr, "###DEBUG A'shipped"
            return 0
        except KeyError:
            print >>sys.stderr, ("ERROR(BAD_CHANNEL)%s%r unknown" %
                                 (SEP, talker_name))
            return BAD_CHANNEL

    def finish_channel():
        msg = Msg(rk=channel_acc[2], config=channel_acc[3], body="\n".join(channel_acc[4:]))
        msg.lineno = channel_acc[1]
        io_group.setdefault(channel_acc[0],[]).append(msg)
        del channel_acc[:]

    def new_channel(channel, rk, config, first_msg_part):
        assert not channel_acc
        channel_acc[:] = [channel, lineno, rk, config, first_msg_part]

    for line in lines:
        lineno += 1
        if line.startswith("#"): continue
        try:
            chan_type, name, msg_opts, msg_part = parse_io_line(line)
            if chan_type:
                if not is_valid_channel(chan_type, name):
                    print >>sys.stderr, (
                        OUT+'ERROR["BAD_CHANNEL"]%s%r' % (SEP, chan_type+name))
                    exit_code |= MALFORMED_INPUT

        except ReplError:
            print >>sys.stderr, (
                OUT+'ERROR["MALFORMED_INPUT"]%s'
                " Bad input! (neither comment nor message):%r" % (SEP, line))
            exit_code |= MALFORMED_INPUT
            continue

        if state == WANT_ANY:
            if msg_part is None:
                assert not line.strip()
                finish_channel()
                exit_code |= ship()
                state = WANT_CHANNEL
            else:
                if not name:
                    channel_acc.append(msg_part)
                else:
                    finish_channel()
                    if chan_type != group_chan_type:
                        exit_code |= ship()
                    new_channel(name,
                                rk=msg_opts.pop('rk', ''),
                                config=msg_opts.pop('config', None),
                                first_msg_part=msg_part)
                    assert not msg_opts
        else:
            assert state == WANT_CHANNEL
            if not name:
                print >>sys.stderr, (
                    OUT+'ERROR["MALFORMED_INPUT"]%s'
                    " need some channel to send message %r to!" % (SEP, msg_part))
                exit_code |= MALFORMED_INPUT
            else:
                new_channel(name,
                            rk=msg_opts.pop('rk', ''),
                            config=msg_opts.pop('config', None),
                            first_msg_part=msg_part)
                assert not msg_opts
                state = WANT_ANY
        group_chan_type = chan_type or group_chan_type

    if channel_acc:
        finish_channel()
        ship()
    return exit_code


if __name__ == '__main__':

    parser = OptionParser("""%prog [OPTIONS] PLUGINPATH [PLUGIN_CONFIG]

    Test a plugin by sending input and observing its output.
    """,
                              [opt(None, "--py", action="store_true",
                                   help="If true, leave control to python, else start I/O repl"),
                               opt(None, "--selftest", action="store_true",
                                   help="Run a basic selftest."),
                               opt(None, "--test", action="store_true",
                                   help="Treat stdin as Input/ExpectedOutput pairings"),
                               opt('-v', "--verbose", action="store_true", default=False,
                                   help="Print out additional info.")
                               ])
    parser.add_option('-D', action="append", dest="env", default=[], help="An environment variable assignment to pass along to the harness")
    parser.add_option("--couchdb",
                      default="http://localhost:5984/",
                      dest="couchdb",
                      help="CouchDB holding configuration")

    amqpoptions = OptionGroup(parser, "AMQP connection parameters")
    amqpoptions.add_option("--amqp-host", default="localhost:5672")
    amqpoptions.add_option("--amqp-user", default="guest")
    amqpoptions.add_option("--amqp-passwd", default="guest")
    amqpoptions.add_option("--amqp-vhost", default="/")
    parser.add_option_group(amqpoptions)

    opts, args = parser.parse_args()
    if opts.selftest:
        import doctest
        print doctest.testmod()
    else:
        if not args or len(args) > 2:
            print >> sys.stderr, "Illegal number of arguments (%d)" % len(args), "try",\
                  sys.argv[0], "--help"
            sys.exit(255)

        plugin_path = args.pop(0)
        filename = args.pop(0) if args else None

        if opts.test:
            is_valid_channel=lambda t, c: (
                t==IN  and (c in wiring.inputs or c == "SLEEP") or
                t==OUT and  c in wiring.outputs)
            Outputter = TestOutputter
            verbosity = 1
            if filename is not None:
                sys.stdin_bak = sys.stdin
                sys.stdin = open(filename)
            make_expect = lambda wiring: wiring.outputter.expect
            field, config_json = sys.stdin.readline().split('\t',1)
            if field != IN+"PLUGIN_INSTANCE_CONFIG":
                print >> sys.stderr, "First line of stdin must be the PLUGIN_INSTANCE_CONFIG"
                sys.exit(255)
            verbosity = 0
        else:
            is_valid_channel=lambda t, c: t==IN and (c in wiring.inputs or c == "SLEEP")
            Outputter = StdoutOutputter if not opts.py else BatchedOutputter
            make_expect = lambda wiring: None
            verbosity = 1 + opts.verbose
            config_json = open(filename).read() if filename else "{}"

        env = [e.partition("=") for e in opts.env]
        env = dict((k,v) for (k,s,v) in env)
        setup_args = dict(plugindir=os.path.abspath(plugin_path),
                          config_json=config_json,
                          env=env,
                          Outputter=Outputter)

        wiring, reconfigure_plugin, config = setup_everything(**setup_args)
        send = wiring.send

        if not opts.py:
            try:
                repl_exit = repl(lines=iter(sys.stdin.readline, ''),
                              send=send,
                              expect=make_expect(wiring),
                              is_valid_channel=is_valid_channel)
                if opts.test:
                    time.sleep(1)
                    if opts.test and opts.verbose:
                        print wiring.outputter
                    sys.exit(255*bool(repl_exit) or min(254, wiring.outputter.failed))
                else:
                    sys.exit(repl_exit)
            finally:
                if hasattr(sys, "stdin_bak"):
                    sys.stdin = sys.stdin_bak

