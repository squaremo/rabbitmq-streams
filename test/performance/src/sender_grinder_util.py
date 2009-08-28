import re
import time

from grinder_util import FeedsTestRunner
from message_set import MessageSet

from net.grinder.script.Grinder import grinder
from net.grinder.script import Test

# The target message rate per second. The implementation of this is crude and
# this figure will actually be a maximum
MSG_RATE = grinder.properties.getDouble("streams.test.msg_rate", 0)

# The file containing the message sets
MESSAGE_SET_FILE = grinder.properties.getProperty("streams.test.message_set_file")

# The regular expression used to extract a message from a line in the message
# set file
extractMsgReStr = grinder.properties.getProperty(
    "streams.test.extract_msg_re",
    r"<label network=\".*\">(.*)</label>")
EXTRACT_MSG_RE = re.compile(extractMsgReStr)

# The regular expression used to insert the message ID into the sent message
insertIdReStr = grinder.properties.getProperty("streams.test.insert_id_re", r"$")
INSERT_ID_RE = re.compile(insertIdReStr)

log = grinder.logger.output

log("Message set file: %s" % MESSAGE_SET_FILE)
log("Message rate: %i tps" % MSG_RATE)

sleepTime = 0
if MSG_RATE > 0:
    sleepTimeSec = 1 / MSG_RATE
    sleepTime = sleepTimeSec * 1000

class SenderTestRunner(FeedsTestRunner):

    def __init__(self, sender):
        self._sender = sender
        self._messageSet = MessageSet(MESSAGE_SET_FILE, EXTRACT_MSG_RE)
        FeedsTestRunner.__init__(self)
        
    def __call__(self):
        t1Ms = time.time() * 1000 

        doTest = Test(1, "Send  message").wrap(self._doTest)
        doTest()

        if sleepTime > 0:
            # Adjust sleep time based on test time
            adjustedSleeptime = sleepTime - ((time.time() * 1000) - t1Ms)
            grinder.sleep(long(adjustedSleeptime))
        

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
