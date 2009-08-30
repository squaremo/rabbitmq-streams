from message_store import MessageStore

from net.grinder.script.Grinder import grinder
from net.grinder.script import Test

MSG_STORE_HOST = grinder.properties.getProperty("streams.test.msg_store.host", "127.0.0.1")
MSG_STORE_PORT = grinder.properties.getInt("streams.test.msg_store.port", 11211)

log = grinder.logger.output

class FeedsTestRunner:

    def __init__(self):
        self._messageStore = MessageStore(MSG_STORE_HOST, MSG_STORE_PORT)
        log("Using message store at %s:%s" % (MSG_STORE_HOST, MSG_STORE_PORT))

    def getMessageStoreHost(self):
        return MSG_STORE_HOST

    def getMessageStorePort(self):
        return MSG_STORE_PORT

    def log(self, message):
        log(message)
