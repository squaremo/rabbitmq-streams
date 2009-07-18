# TODO: Some comments
# TODO: Make more objecty
# TODO: Stop multiple threads from calling
# TODO: Real delivery time
# TODO: Use message formatting
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

# TODO: Make into script parameter
PORT=12345

log = grinder.logger.output
totalRuns = grinder.properties.getInt("grinder.runs", -1)

# Shared lock
lock = Condition()

# Shared message queue
messageQueue = []

# Register custom statistics - Delivery time and Mean delivery time
grinder.statistics.registerDataLogExpression("Delivery time", "userLong0")
grinder.statistics.registerSummaryExpression("Mean delivery time",
                                             "(/ userLong0 (count timedTests))")


# Test timing is meaningless for this scenario, record the delivery time instead
def recordDeliveryTime(deliveryTime):
    grinder.statistics.forCurrentTest.setLong("userLong0", deliveryTime)

recordTest = Test(1, "Receive messages").wrap(recordDeliveryTime)

class NetworkListener(Thread):
    # TODO: Comments
    
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
        """Called for each message received. Adds the timing information to the
        message queue in a thread safe manner."""
        lock.acquire()
        sendTime = long(message)
        deliveryTime = System.currentTimeMillis() - sendTime
        messageQueue.append(deliveryTime)
        lock.notifyAll()
        lock.release()

    def stop(self):
        """Stop the server socket and close any open client socket"""
        self._isListening = False

        if(self._serverSocket is not None):
            self._serverSocket.close()

        if(self._clientSocket is not None):
            self._clientSocket.close() 
        

listener = NetworkListener()
listener.start()

class TestRunner:

    def __call__(self):
        lock.acquire()
        while not messageQueue: lock.wait()
        deliveryTime = messageQueue.pop(0)
        lock.release()
        recordTest(deliveryTime)

        if(grinder.runNumber >= totalRuns - 1):
            listener.stop()
