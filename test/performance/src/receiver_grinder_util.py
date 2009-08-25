from net.grinder.script.Grinder import grinder
from net.grinder.script import Test

from grinder_util import FeedsTestRunner

# Register custom statistics
grinder.statistics.registerDataLogExpression("Received time", "userLong0")

grinder.statistics.registerDataLogExpression("Delivery time (ms)", "userLong1")
grinder.statistics.registerSummaryExpression("Mean delivery time (ms)",
                                             "(/ userLong1 (count timedTests))")

class ReceiverTestRunner(FeedsTestRunner):

    def __init__(self, receiver):
        self._receiver = receiver
        FeedsTestRunner.__init__(self)

    def __call__(self):
        doTest = Test(1, "Receive messages").wrap(self._doTest)
        doTest()

    def _doTest(self):
        done = False
        
        while(not done):
            # Get next received message, blocking if necessary
            msg, receivedT = self._receiver.getMessage()

            print "Got " + str(msg) + ", " + str(long(receivedT))

            # Get from message store
            msgId = self._getMessageId(msg)
            sentT = self._messageStore.get(msgId)

            if sentT is None:
                print "Putting message back"
                self._receiver.putBackMessage(msgId, receivedT)
            else:
                print "Recording message"
                deliveryT = receivedT - sentT

                self._recordReceivedTime(long(receivedT))
                self._recordDeliveryTime(long(deliveryT))

                done = True

    def _recordReceivedTime(self, receivedTime):
        grinder.statistics.forCurrentTest.setLong("userLong0", receivedTime)
        
    def _recordDeliveryTime(self, deliveryTime):
        grinder.statistics.forCurrentTest.setLong("userLong1", deliveryTime)
