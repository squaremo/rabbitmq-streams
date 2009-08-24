from receiver import NetworkReceiver
from message_store import MessageStore
from receiver_grinder_util import ReceiverTestRunner


class TestRunner(ReceiverTestRunner):
    def __init__(self):
        import time

        ms = MessageStore('127.0.0.1', '11211')
        sentT = time.time() * 1000
        print "initial sentT: " + str(long(sentT))
        ms.set('1', sentT)
        
        receiver = NetworkReceiver(12345)
        receiver.start()
        ReceiverTestRunner.__init__(self, receiver)



