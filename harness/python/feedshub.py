"""
Interfaces for plugin components to use.
"""

from __future__ import with_statement

import couchdb.client as couch
import amqplib.client_0_8 as amqp
import os
import sys
import threading

feedshub_log_xname = 'feedshub/log'
plugin_config_header = 'x-streams-plugin-config'

try:
    import simplejson as json
except:
    import json

def amqp_connection_from_config(hostspec):
    hostname = hostspec['host']
    port = str(hostspec['port'])
    host = ":".join([hostname, port])
    virt = hostspec['virtual_host']
    userid, password = hostspec['username'], hostspec['password']
    connection = amqp.Connection(host=host,
                                 userid=userid,
                                 password=password,
                                 virtual_host=virt)
    return connection

def pp_message(msg):
    d = {"body": msg.body,
         "properties": msg.properties}
    if hasattr(msg, 'delivery_info'):
        d["message_kind"] = "delivery"
        # channel.py just pokes the delivery_info attribute straight
        # onto the Message, rather than defining some subclass or
        # similar
        d["delivery_info"] = dict(msg.delivery_info)
        del d["delivery_info"]["channel"]
    else:
        d["message_kind"] = "publication"
    return d

def pformat(j):
    return json.dumps(j, indent=2)

# def pformat(j):
#     import pprint
#     return pprint.pformat(j)

class PluginBase(object):
    """Base class for plugins -- either pipeline components, or ingress/egress servers.

    Note on __publication_error: Sometimes, amqplib decides to throw
    in the middle of sending a command (such as basic.publish). If
    this happens, the server will never see the rest of the command,
    which puts the channel in an unusable condition. Therefore, we
    detect when it's *possible* that *some* channel is in an unusable
    state, and if it's at all possible, we log exceptions to stderr
    instead of to the AMQP log channel, so that we don't lose
    important information on exceptions that occur.
    """
    INPUTS = {}
    OUTPUTS = {}

    def __init__(self, config):
        self.__monitor = threading.RLock()

        msghostspec = config['messageserver']
        self.__conn = amqp_connection_from_config(msghostspec)
        self.__channel = self.__conn.channel()
        self.__channel.tx_select()

        self.__publication_error = False
        self.__log = self.__conn.channel() # a new channel which isn't tx'd
        self.build_logger(config)

        if 'database' in config and config['database'] is not None:
            self.__db = couch.Database(config['database'])
        else:
            self.__db = None

        settings = dict((item['name'], item['value'])
                        for item in config['plugin_type']['global_configuration_specification'])
        settings.update(config['configuration'])
        self._static_config = settings

        for name, exchange in config['outputs'].iteritems():
            if name not in self.OUTPUTS:
                self.OUTPUTS[name] = name
            setattr(self, self.OUTPUTS[name],
                    self._make_exchange_publisher(self.__channel, exchange, ''))

        def handler(fun):
            def handle(msg):
                if 'application_headers' in msg.properties:
                    headers = msg.properties['application_headers']
                    if headers and plugin_config_header in headers:
                        config = headers[plugin_config_header]
                        self.debug("Plugin config found: " + config)
                        dynamic = {}
                        dynamic.update(self._static_config)
                        try:
                            headerConfig = json.loads(config)
                            if not isinstance(headerConfig, dict):
                                raise "Not a dict"
                        except:
                            self.error("Could not use config: " + config + "; ignoring message")
                            return
                        dynamic.update(headerConfig)
                        return fun(msg, dynamic)
                return fun(msg, self._static_config)
            return handle

        for name, queue in config['inputs'].iteritems():
            if name not in self.INPUTS:
                self.INPUTS[name] = name
            method = getattr(self, self.INPUTS[name])
            self._subscribe_to_queue(queue, handler(method))

        self.info(pformat({"event": "configured",
                           "args": {"config": settings}}))

    def monitor(self):
        return self.__monitor

    def _make_exchange_publisher(self, channel, exchange, routing_key):
        def p(body, **headers):
            override_routing_key = headers.pop("override_routing_key", None)
            if self.__publication_error:
                raise Exception("Publishing after publication error")
            message = amqp.Message(body=body, children=None, delivery_mode=2, **headers)
            # TODO: treat application_headers specially, and expect a content type
            with self.__monitor:
                try:
                    channel.basic_publish(message, exchange,
                                          override_routing_key or routing_key)
                except:
                    self.__publication_error = True
                    raise

        return p

    def _subscribe_to_queue(self, queue, method):
        def h(msg):
            with self.__monitor:
                try:
                    method(msg)
                    self.__channel.basic_ack(msg.delivery_info['delivery_tag'])
                    self.__channel.tx_commit()
                except Exception:
                    self.__channel.tx_rollback()
                    self.log_exception("Exception when trying to handle an input from queue " +
                                       queue + " to method " + str(method) + "\n" +
                                       pformat(pp_message(msg)))
                    self.terminate()

        # no_ack: If this field is set the server does not expect
        # acknowledgements for messages.
        self.__channel.basic_consume(queue=queue, no_ack=False, callback=h)

    def run_input_handler(self, func):
        with self.__monitor:
            try:
                result = func()
                self.__channel.tx_commit()
                return result
            except:
                self.__channel.tx_rollback()
                raise

    def log_exception(self, errMsg, log_to_stderr = False):
        import traceback
        formatted = str(errMsg) + "\n" + traceback.format_exc()
        if log_to_stderr or self.__publication_error:
            print >> sys.stderr, formatted
        else:
            try:
                self.error(formatted)
            except Exception:
                second_formatted = \
                    formatted + \
                    "\n...additionally, an exception in self.error was reported\n" + \
                    traceback.format_exc()
                print >> sys.stderr, second_formatted

    def terminate(self):
        sys.exit(1)

    def commit(self):
        with self.__monitor:
            self.__channel.tx_commit()

    def rollback(self):
        with self.__monitor:
            self.__channel.tx_rollback()

    def build_logger(self, config):
        def logger(level):
            rk = self._build_log_rk(config, level)
            publisher = self._make_exchange_publisher(self.__log, feedshub_log_xname, rk)
            def log(body, label=None):
                headers = {"com.rabbitmq.streams.logging.label": label} if label else {}
                publisher(body, **headers)
            return log
        for level in ['debug', 'info', 'warn', 'error', 'fatal']:
     	    setattr(self, level, logger(level))

    def privateDatabase(self):
        return self.__db

    def setting(self, name, defaultValue = None):
        """Get a configuration setting."""
        return self.__config.get(name, defaultValue)

    def run(self):
        while True:
            self.__channel.wait()

    def start(self):
        try:
            self.run()
        except Exception:
            self.log_exception("Exception in AMQP connection thread", True)
            self.terminate()

class Component(PluginBase):
    def __init__(self, config):
        super(Component, self).__init__(config)
        self.__stateresource = couch.Resource(None, config['state'])

    def _build_log_rk(self, config, level):
        feed_id = config['feed_id']
        plugin_name = config['plugin_name']
        node_id = config['node_id']
        return level + '.' + feed_id + '.' + plugin_name + '.' + node_id

    def putState(self, state):
        """Record the state of the component"""
        #print "Putting state: "
        #print json.dumps(state)
        resp, data = self.__stateresource.put(content = state)
        return self.getState()

    def getState(self, defaultState = None):
        try:
            resp, data = self.__stateresource.get()
            return couch.Document(data)
        except couch.ResourceNotFound:
            return defaultState

class Server(PluginBase):
    def __init__(self, config):
        super(Server, self).__init__(config)
        self.__server_id = config['server_id']
        self.__terminalsDb = couch.Database(config['terminals_database'])

    def _build_log_rk(self, config, level):
        server_id = config['server_id']
        plugin_name = config['plugin_name']
        return level + '.' + server_id + '.' + plugin_name

    def getServerId(self):
        return self.__server_id

    def command(self, msg):
        ## self.debug(pformat(pp_message(msg)))
        (server_id, terminal_id) = msg.delivery_info["routing_key"].rsplit('.', 1)
        terminal_configs = self._configs_for_terminal(terminal_id)
        self.terminal_status_change(terminal_id,
                                    terminal_configs,
                                    self._terminal_status(terminal_id))

    def _configs_for_terminal(self, terminal_id):
        terminal_config = self.__terminalsDb[terminal_id]
        servers = terminal_config["servers"]
        return [c for c in servers if c["server"] == self.__server_id]

    def _terminal_status(self, terminal_id):
        return self.__terminalsDb[terminal_id + "_status"]["active"]

    def terminal_status_change(self, terminal_id, terminal_configs, terminal_is_active):
        self.info(pformat({"event": "terminal_status_change",
                           "args": {"terminal_id": terminal_id,
                                    "terminal_configs": terminal_configs,
                                    "terminal_is_active": terminal_is_active}}))
