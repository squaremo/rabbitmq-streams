import time

from threading import Condition

class TimestampedQueue:
    """Thread safe queue implementation that saves a timestamp with each queue
    item"""
    
    def __init__(self):
        self._lock = Condition()
        self._queue = []

    def append(self, item, timestampMs = -1):
        """Append an item to the queue"""

        # Default value is set at time of function definition so must do time
        # assignment separately
        if timestampMs == -1:
            timestampMs = time.time() * 1000
        
        self._lock.acquire()
        self._queue.insert(0, [item, timestampMs])
        self._lock.notifyAll()
        self._lock.release()

    def pop(self):
        """Pops an item from the queue, or blocks until one is available.
        The item is returned in a list consisting of the item itself, and a
        timestamp in ms of when the item was put onto the queue."""
        self._lock.acquire()
        while not self._queue:
            self._lock.wait()
        message = self._queue.pop()
        self._lock.release()
        return message
        
