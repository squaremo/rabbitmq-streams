import time
import Queue

class TimestampedQueue:
    """Thread safe queue implementation that saves a timestamp with each queue
    item"""
    
    def __init__(self):
        self._queue = Queue.Queue(0)

    def append(self, item, timestampMs = -1):
        """Append an item to the queue"""

        # Default value is set at time of function definition so must do time
        # assignment separately
        if timestampMs == -1:
            timestampMs = time.time() * 1000
        # this next won't block, since the queue size is infinite
        self._queue.put([item, timestampMs])

    def pop(self):
        """Pops an item from the queue, or blocks until one is available.
        The item is returned in a list consisting of the item itself, and a
        timestamp in ms of when the item was put onto the queue."""
        return self._queue.get()
