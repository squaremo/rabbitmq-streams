from receiver import NetworkReceiver
from receiver_grinder_util import ReceiverTestRunner
from net.grinder.script.Grinder import grinder

PORT = grinder.properties.getInt("streams.test.port", 55555)

class TestRunner(ReceiverTestRunner):
    def __init__(self):
        receiver = NetworkReceiver(PORT)
        receiver.start()
        ReceiverTestRunner.__init__(self, receiver)



