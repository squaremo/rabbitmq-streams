# TODO: Some comments
# TODO: Make more objecty
# TODO: Stop multiple threads from calling
# TODO: Currently takes only one connx at a time

from net.grinder.script.Grinder import grinder
from net.grinder.script import Test

from jarray import zeros

from java.io import BufferedReader
from java.io import InputStreamReader
from java.lang import System
from java.net import ServerSocket
from java.net import Socket
from java.net import SocketException
from java.util import Random
from threading import Condition
from threading import Thread
from net.grinder.script.Grinder import grinder

import re

PORT = grinder.properties.getInt("streams.test.port", 55555)

log = grinder.logger.output
totalRuns = grinder.properties.getInt("grinder.runs", -1)

# Shared lock
lock = Condition()

# Shared message queue
messageQueue = []

# Register custom statistics - Delivery time and Mean delivery time
grinder.statistics.registerDataLogExpression("Delivery time", "userLong0")
grinder.statistics.registerSummaryExpression("Mean delivery time (ms)",
                                             "(/ userLong0 (count timedTests))")

# Compile the regex pattern outside of a class method
# See http://osdir.com/ml/java.grinder.user/2005-11/msg00060.html
tsPattern = re.compile(r"\.*|timestamp=([0-9]*)\|")

# Test timing is meaningless for this scenario, record the delivery time instead
def recordDeliveryTime(deliveryTime):
    grinder.statistics.forCurrentTest.setLong("userLong0", deliveryTime)

recordTest = Test(1, "Receive messages").wrap(recordDeliveryTime)

class NetworkListener(Thread):
    """Use a new thread to listen to a network port and place each message
    sent on a new line to the message queue"""
    
    def __init__(self):
        self._serverSocket = ServerSocket(PORT)
        self._isListening = True
        Thread.__init__(self)
        log("Started server on " + str(PORT))
        
    def run(self):
        """Start thread listening to the socket"""
        while(self._isListening):
            self._clientSocket = self._serverSocket.accept()
            self._handleConnection(self._clientSocket)
        
    def _handleConnection(self, socket):
        reader = BufferedReader(InputStreamReader(socket.getInputStream()))

        try:
            line = reader.readLine();
    
            while(line is not None and not self._serverSocket.closed):
                self._handleMessage(line)
                line = reader.readLine();
        except SocketException:
            if self._isListening:
                raise SocketException
            else:
                log("Ignoring socket exception during shutdown")

        socket.close()

    def _handleMessage(self, message):
        """Called for each message received."""
        lock.acquire()
        messageQueue.append(message)
        lock.notifyAll()
        lock.release()

    def stop(self):
        """Stop the server socket and close any open client socket"""
        self._isListening = False

        if(self._serverSocket is not None):
            self._serverSocket.close()

        if(self._clientSocket is not None):
            self._clientSocket.close() 

class TestRunner:
    def __init__(self):
        if grinder.threadNumber > 0:
            raise RuntimeError("Can only support one thread")

        grinder.logger.output("Thread starting")
        self.initialisationTime = System.currentTimeMillis()
        self._listener = NetworkListener()
        self._listener.start()

    def __call__(self):
        lock.acquire()
        while not messageQueue: lock.wait()
        message = messageQueue.pop(0)
        lock.release()
        
        m = tsPattern.match(message)
        if m is None or not len(m.groups()) ==1:
            raise Exception(
                "Could not find timestamp in message: " + str(message))

        timestamp = long(m.group(1))
        deliveryTime = System.currentTimeMillis() - timestamp
        recordTest(deliveryTime)

        if(totalRuns > 0 and grinder.runNumber >= totalRuns - 1):
            self._listener.stop()

    def __del__(self):
        grinder.logger.output("Thread shutting down")
        self._listener.stop()
