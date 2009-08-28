import time
import unittest

from threading import Event
from threading import Thread

import queue


TEST_ITEM = "TEST_ITEM"

class testTimestampedQueue(unittest.TestCase):
    def setUp(self):
        self._queue = queue.TimestampedQueue()
    
    def testSimpleCase(self):
        self._queue.append(TEST_ITEM)
        val = self._queue.pop()
        self.assertEquals(TEST_ITEM, val[0])

        # Test within 10s
        self.assertAlmostEqual(time.time() * 1000, val[1], -4)

    def testInsert(self):
        self._queue.append(1)
        self._queue.append(2)
        self._queue.append(3)
        self.assertEquals(1, self._queue.pop()[0])
        self._queue.append(1, 55555)
        self.assertEquals(2, self._queue.pop()[0])
        self.assertEquals(3, self._queue.pop()[0])

        msgId, ts = self._queue.pop()
        self.assertEquals(1, msgId)
        self.assertEquals(55555, ts)


    def testBlocking(self):
        event = Event()
        getter = GetItemProcess(self._queue, event)
        getter.start()
        time.sleep(1)
        self.assertFalse(event.isSet())
        self._queue.append(TEST_ITEM)
        event.wait(5)
        self.assertTrue(event.isSet())

class GetItemProcess(Thread):
    """Test helper that gets an item in a new thread which blocks, and then
    sets the flag on the shared event object when the attempt succeeds"""
    
    def __init__(self, queue, event):
        self._queue = queue
        self._event = event
        Thread.__init__(self)
        
    def run(self):
        msg = self._queue.pop()
        if msg:
            self._event.set()


if __name__ == '__main__':
    unittest.main()

