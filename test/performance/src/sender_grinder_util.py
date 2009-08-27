import re
import time

from grinder_util import FeedsTestRunner
from message_set import MessageSet

#from receiver import NetworkReceiver
#from message_store import MessageStore

#from net.grinder.script.Grinder import grinder
from net.grinder.script import Test


#PORT = grinder.properties.getInt("streams.test.port", 55555)

#MSG_STORE_HOST = grinder.properties.getProperty("streams.test.msg_store.host", "127.0.0.1")
#MSG_STORE_PORT = grinder.properties.getInt("streams.test.msg_store.port", 11211)

#log = grinder.logger.output
#log("Port: %i" % PORT)

# TODO: Var
EXTRACT_MSG_RE = re.compile('<label network=".*">(.*)</label>')

# TODO: Var
INSERT_ID_RE = re.compile('$')


class SenderTestRunner(FeedsTestRunner):

    def __init__(self, sender, messageFile):
        self._sender = sender
        self._messageSet = MessageSet(messageFile, EXTRACT_MSG_RE)
        FeedsTestRunner.__init__(self)
        
    def __call__(self):
        doTest = Test(1, "Send  message").wrap(self._doTest)
        doTest()

    def _doTest(self):
        msg = self._messageSet.next()
        msgId = self._getMessageId(msg)
        msg = self._insertMessageId(msgId, msg)
        self._sender.send(msg + '\n')
        self._messageStore.set(msgId, time.time() * 1000)

    def _insertMessageId(self, msgId, message):
        # TODO: Implement regex based ID inserter
        return msgId + message

    def _getMessageId(self, message):
        return message[:1]
