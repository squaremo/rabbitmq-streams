import queue
import receiver
import socket
import time
import unittest

TEST_MESSAGE = "TEST_MESSAGE"

LOCALHOST = '127.0.0.1'

TEST_PORT = 56789

class testReceiver(unittest.TestCase):
    def setUp(self):
        self._receiver = receiver.Receiver()
    
    def testSimpleCase(self):
        self._receiver.receiveMessage(TEST_MESSAGE)
        val = self._receiver.getMessage()
        self.assertEquals(TEST_MESSAGE, val[0])

class testNetworkReceiver(unittest.TestCase):
    def setUp(self):
        print "setUp"
        self._receiver = receiver.NetworkReceiver(TEST_PORT)
        self._receiver.start()
        self._socket = self._connectSocket()

    def testSingleMsgLF(self):
        self._socket.send('FOO\n')
        self.assertEquals('FOO', self._receiver.getMessage()[0])

    def testSingleMsgCRLF(self):
        self._socket.send('FOO\r\n')
        self.assertEquals('FOO', self._receiver.getMessage()[0])

    def testMultipleCompleteMsg(self):
        self._socket.send('FOO\nBAR\nBAZ\n')
        self.assertEquals('FOO', self._receiver.getMessage()[0])
        self.assertEquals('BAR', self._receiver.getMessage()[0])
        self.assertEquals('BAZ', self._receiver.getMessage()[0])

    def testMultipleIncompleteMsg(self):
        self._socket.send('FOO\nBAR\nBAZ')
        self.assertEquals('FOO', self._receiver.getMessage()[0])
        self.assertEquals('BAR', self._receiver.getMessage()[0])
        self._socket.send('FOO2\n')
        self.assertEquals('BAZFOO2', self._receiver.getMessage()[0])

    def testMultipleClients(self):
        s2 = self._connectSocket()
        self._socket.send("START_CLIENT_1_")
        s2.send("START_CLIENT_2_")
        self._socket.send("END_CLIENT_1\n")
        s2.send("END_CLIENT_2\n")

        receivedMsgs = [self._receiver.getMessage()[0], self._receiver.getMessage()[0]]

        # remove() will throw an error of the item is not present
        receivedMsgs.remove('START_CLIENT_1_END_CLIENT_1')
        receivedMsgs.remove('START_CLIENT_2_END_CLIENT_2')

        s2.close()
        
    def tearDown(self):
        self._socket.close()
        self._receiver.stop()
        self._receiver.join()

    def _connectSocket(self):
        s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        s.connect((LOCALHOST, TEST_PORT))
        return s

if __name__ == '__main__':
    unittest.main()

