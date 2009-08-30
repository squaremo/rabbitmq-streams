from receiver import NetworkReceiver
from receiver_grinder_util import ReceiverTestRunner


class TestRunner(ReceiverTestRunner):
    def __init__(self):
        receiver = NetworkReceiver(12345)
        receiver.start()
        ReceiverTestRunner.__init__(self, receiver)



