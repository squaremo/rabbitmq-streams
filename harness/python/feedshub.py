"""
Interfaces for plugin components to use.
"""

import couchdb.client as couch
import amqplib.client_0_8 as amqp
import os
import sys
import pprint

feedshub_log_xname = 'feedshub/log'

try:
    import simplejson as json
except:
    import json


def db_name(db):
    """Give the name of the database.
    >>> db = db_from_config(dict(server='http://localhost:5984', database='pythontest'))
    >>> db_name(db)
    "pythontests"
    """
    return db._name

def db_server(db):
    """Give the server part of a database.
    >>> db = db_from_config(dict(server='http://localhost:5984', database='pythontest'))
    >>> db_server(db)
    "http://localhost:5984"
    """
    return db.resource.uri[:-len(db._name)]

def config_of_db(db):
    """Give the configuration stanza of a store
    >>> db = db_from_config(dict(server='http://localhost:5984', database='pythontests'))
    >>> config = config_of_db(db)
    >>> config['server']
    "http://localhost:5984"
    >>> config['database']
    "pythontests"
    """
    return dict(database=db_name(db), server=db_server(db))

def db_from_config(config):
    """Make a store given a config.
    >>> db = db_from_config(dict(server='http://localhost:5984', database='pythontests'))
    >>> db_name(db)
    "pythontests"
    >>> db_server(db)
    "http://localhost:5984"
    """
    server = couch.Server(config['server'])
    return server[config['database']]

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
        d["delivery_info"] = msg.delivery_info
    else:
        d["message_kind"] = "publication"
    return d

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
        msghostspec = config['messageserver']
        self.__conn = amqp_connection_from_config(msghostspec)
        self.__channel = self.__conn.channel()
        self.__channel.tx_select()

        self.__publication_error = False
        self.__log = self.__conn.channel() # a new channel which isn't tx'd
        self.build_logger(config)
        self.info('Starting up...')

        if 'database' in config and config['database'] is not None:
            self.__db = couch.Database(config['database'])
        else:
            self.__db = None

        settings = dict((item['name'], item['value'])
                        for item in config['plugin_type']['global_configuration_specification'])
        settings.update(config['configuration'])
        self.__config = settings

        for name, exchange in config['outputs'].iteritems():
            if name not in self.OUTPUTS:
                self.OUTPUTS[name] = name
            setattr(self, self.OUTPUTS[name],
                    self._make_exchange_publisher(self.__channel, exchange, ''))

        for name, queue in config['inputs'].iteritems():
            if name not in self.INPUTS:
                self.INPUTS[name] = name
            method = getattr(self, self.INPUTS[name])
            self._subscribe_to_queue(self.__channel, queue, method)

    def _make_exchange_publisher(self, channel, exchange, routing_key):
        def p(body, **headers):
            if self.__publication_error:
                raise Exception("Publishing after publication error")
            message = amqp.Message(body=body, children=None, delivery_mode=2, **headers)
            # TODO: treat application_headers specially, and expect a content type
            try:
                channel.basic_publish(message, exchange, routing_key)
            except:
                self.__publication_error = True
                raise

        return p

    def _subscribe_to_queue(self, channel, queue, method):
        def h(msg):
            try:
                method(msg)
                channel.basic_ack(msg.delivery_info['delivery_tag'])
                channel.tx_commit()
            except Exception:
                channel.tx_rollback()
                self.log_exception("Exception when trying to handle an input from queue " +
                                   queue + " to method " + str(method) +
                                   pprint.pformat(pp_message(msg)),
                                   False)

        # no_ack: If this field is set the server does not expect
        # acknowledgements for messages.
        channel.basic_consume(queue=queue, no_ack=False, callback=h)

    def log_exception(self, errMsg, log_to_stderr):
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
        self.terminate()

    def terminate(self):
        sys.exit(1)

    def commit(self):
        self.__channel.tx_commit()

    def rollback(self):
        self.__channel.tx_rollback()

    def build_logger(self, config):
        for level in ['debug', 'info', 'warn', 'error', 'fatal']:
            rk = self._build_log_rk(config, level)
            setattr(self, level,
                    self._make_exchange_publisher(self.__log, feedshub_log_xname, rk))

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
        self.__terminalsDb = couch.Database(config['terminals_database'])

    def _build_log_rk(self, config, level):
        server_id = config['server_id']
        plugin_name = config['plugin_name']
        return level + '.' + server_id + '.' + plugin_name

    def command(self, msg):
        # (server_id, terminal_id) = msg.delivery_info["routing_key"].rsplit('.', 1)
        self.debug(pprint.pformat(pp_message(msg)))
