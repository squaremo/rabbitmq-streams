import unittest

from message_store import MessageStore

TEST_MESSAGE_ID = "TEST_MESSAGE_ID"
TEST_TIMESTAMP = 12345

class testMessageStore(unittest.TestCase):
    """Tests the message store. Note that the underlying store, e.g.,
    memcached, must be available before running these tests."""
    def setUp(self):
        self._ms = MessageStore('127.0.0.1', '11211')
        
    def testSetAndGet(self):
        self._ms.set(TEST_MESSAGE_ID, TEST_TIMESTAMP)
        self.assertEqual(TEST_TIMESTAMP, self._ms.get(TEST_MESSAGE_ID))

    def testGetEmpty(self):
        self.assertEquals(None, self._ms.get(TEST_MESSAGE_ID))
        pass

if __name__ == '__main__':
    unittest.main()
