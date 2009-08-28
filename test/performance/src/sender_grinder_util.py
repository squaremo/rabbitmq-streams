import re
import time

from grinder_util import FeedsTestRunner
from message_set import MessageSet

from net.grinder.script.Grinder import grinder
from net.grinder.script import Test

extractMsgReStr = grinder.properties.getProperty("streams.test.extract_msg_re",
                                                 r"<label network=\".*\">(.*)</label>")
EXTRACT_MSG_RE = re.compile(extractMsgReStr)

insertIdReStr = grinder.properties.getProperty("streams.test.insert_id_re", r"$")
INSERT_ID_RE = re.compile(insertIdReStr)

MESSAGE_SET_FILE = grinder.properties.getProperty("streams.test.message_set_file")


class SenderTestRunner(FeedsTestRunner):

    def __init__(self, sender):
        self._sender = sender
        self._messageSet = MessageSet(MESSAGE_SET_FILE, EXTRACT_MSG_RE)
        FeedsTestRunner.__init__(self)
        
    def __call__(self):
        doTest = Test(1, "Send  message").wrap(self._doTest)
        doTest()

    def _doTest(self):
        msg = self._messageSet.next()
        msgId = self._getMessageId(msg)
        msg = self._insertMessageId(msgId, msg)

        sentT = time.time() * 1000
        self._sender.send(msg + '\n')
        self._messageStore.set(msgId, sentT)

    def _insertMessageId(self, msgId, message):
        msgIdStr = "||MSG_ID=%s||" % msgId
        return re.sub(INSERT_ID_RE, msgIdStr, message)

    def _getMessageId(self, message):
        """Create a message ID based on the agent, process, thread and run
        number. This should be unique across multiple test clients."""
        msgIdInts = [ grinder.agentNumber, grinder.processNumber,
                 grinder.threadNumber, grinder.runNumber ]
        msgIdStrs = map(lambda item : str(item), msgIdInts)
        return ''.join(msgIdStrs)
