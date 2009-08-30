import memcache

DEFAULT_IP_ADDRESS = "127.0.0.1"
DEFAULT_PORT = 11211

class MessageStore:
    """Message store that uses a memcached instance as a backing store"""
    def __init__(self, ipAddress, port):
        if ipAddress == None: ipAddress = DEFAULT_IP_ADDRESS
        if port == None: port = DEFAULT_PORT
        self._mc = memcache.Client([ipAddress + ':' + str(port)], debug=0)

    def set(self, messageId, timeMs):
        self._mc.set(str(messageId), str(timeMs))
        
    def get(self, messageId):
        timeMs = self._mc.get(str(messageId))
        if timeMs == None:
            return None
        else:
            self._mc.delete(str(messageId))

        return float(timeMs)
