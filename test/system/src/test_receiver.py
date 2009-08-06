import time
import unittest

from threading import Event
from threading import Thread

import receiver


MESSAGE = "This is a test message"

class testMessageQueue(unittest.TestCase):


    def setUp(self):
        self._queue = receiver.MessageQueue()
    
    def testSimpleCase(self):
        self._queue.append(MESSAGE)
        self.assertEquals(MESSAGE, self._queue.pop())

    def testBlocking(self):
        event = Event()
        getter = GetMessageProcess(self._queue, event)
        getter.start()
        time.sleep(1)
        self.assertFalse(event.isSet())
        self._queue.append(MESSAGE)
        event.wait(5)
        self.assertTrue(event.isSet())

class GetMessageProcess(Thread):
    """Gets a message in a new thread which blocks, and then sets the flag on
    the shared event object when the attempt succeeds"""
    
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

