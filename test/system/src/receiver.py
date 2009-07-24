from threading import Condition

class MessageQueue():
    """Thread safe queue implementation"""
    
    def __init__(self):
        self._lock = Condition()
        self._messageQueue = []

    def append(self, message):
        self._lock.acquire()
        self._messageQueue.append(message)
        self._lock.notifyAll()
        self._lock.release()

    def pop(self):
        self._lock.acquire()
        while not self._messageQueue:
            self._lock.wait()
        message = self._messageQueue.pop(0)
        self._lock.release()
        return message
