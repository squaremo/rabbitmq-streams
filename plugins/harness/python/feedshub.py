"""
Interfaces for components to use.
"""

import couchdb.client as couch
import stomp

try:
    import json
except:
    import simplejson as json

def cons_db(server):
    """Construct a storage database
    @return the fresh database
    """
    import os, sha
    # TODO How much random do we need here?
    dbname = sha.new(os.urandom(8)).hexdigest()
    s = couch.Server(server)
    while dbname in s:
        dbname = sha.new(os.urandom(8)).hexdigest()
    s.create(dbname)
    return db_from_config(dict(server=server, database=dbname))

def ensure_db(config):
    s = couch.Server(config['server'])
    dbname = config['database']
    if dbname not in s:
        s.create(dbname)

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

class StompPublisher: # FIXME supply the connection
    def __init__(self, connection, exchange, key):
        self.__key = key
        self.__exchange = exchange
        self.__conn = connection

    def publish(self, msg):
        self.__conn.send(message=json.dumps(msg),
                         exchange=self.__exchange,
                         destination=self.__key)

def connection_from_config(hostspec):
    hostname = hostspec['host']
    port = int(hostspec['port'])
    login, passcode = hostspec['username'], hostspec['password'] # what are these called in AMQP?
    connection = stomp.Connection([(hostname, port)], user=login, passcode=passcode)
    return connection

def publisher_from_config(connection, exchangespec):
    exchange = exchangespec['exchange']
    destination = exchangespec.get('routingkey', '')
    return StompPublisher(connection, exchange, destination)

class Source:
    """
    A source has only an output of messages, generated on some schedule.

    Subclasses get to use
     - emit() to send an item
     - putState() and getState() to keep state information
     - error() to report an error in processing
     - setting() to retrieve a configuration setting
    """

    def __init__(self, config):
        channelspec = config['channels']
        msghostspec = channelspec['host']
        self.__conn = connection_from_config(msghostspec)
        self.__conn.start()
        self.__conn.connect()
        emitspec = channelspec['out']
        self.__out = publisher_from_config(self.__conn, emitspec)
        errorspec = channelspec['err']
        self.__err = publisher_from_config(self.__conn, errorspec)
        statespec = config['state']
        self.__statedb = db_from_config(statespec)
        self.__statedoc = statespec['documentid']
        self.__settings = config['settings']
        
    def putState(self, state):
        """Record the state of the component"""
        print "Putting state: "
        print json.dumps(state)
        self.__statedb[self.__statedoc] = state

    def getState(self):
        return self.__statedb[self.__statedoc]

    def setting(self, name):
        """Get a configuration setting."""
        return self.__settings.get(name, None)

    def publish(self, msg):
        """Send an item into the system"""
        self.__out.publish(msg)

    def error(self, msg):
        """Report a problem"""
        self.__err.publish(msg)

    def start(self):
        try:
            self.run()
        finally:
            if self.__conn.is_connected():
                self.__conn.disconnect()
