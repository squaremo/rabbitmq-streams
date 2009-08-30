import queue
import re
import select
import socket

from threading import Condition
from threading import Thread


BIND_ADDRESS = '0.0.0.0'

CRLF_RE = '\r?\n'

# Compile the regex patterns outside of a class method
# See http://osdir.com/ml/java.grinder.user/2005-11/msg00060.html
messageTokenizeRe = re.compile(CRLF_RE)
messageCompletenessRe = re.compile(CRLF_RE + '$')

def consoleLog(msg):
    print msg

class Receiver:
    """Parent class of all receivers"""
    def __init__(self, logFunction = consoleLog):
        self._log = logFunction
        self._messageQueue = queue.TimestampedQueue()

    def receiveMessage(self, message):
        self._messageQueue.append(message)

    def getMessage(self):
        return self._messageQueue.pop()

    def putBackMessage(self, message, timestamp):
        self._messageQueue.append(message, timestamp)


class NetworkReceiver(Thread, Receiver):
    def __init__(self, port, logFunction = consoleLog):
        backlog = 5
        self._serverSocket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)

        # Jython wants the socket in non-blocking to run select()
        self._serverSocket.setblocking(0)

        # Re-use the address to stop bind problems when running lots of tests
        self._serverSocket.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
        self._serverSocket.bind((BIND_ADDRESS, port))

        self._serverSocket.listen(backlog)
        self._clientBuffers = {}
            
        Thread.__init__(self)
        Receiver.__init__(self, logFunction)

        self._isListening = True
        self._log("Started server on " + str(port))
        
    def run(self):
        """Start thread listening to the socket"""
        bufferSize = 1024
        inputSockets = [self._serverSocket]
        
        while(self._isListening):
            inputReady, outputReady, exceptReady = select.select(
                inputSockets,[],[], 0)

            for sock in inputReady:
                if sock == self._serverSocket:
                    # Client socket connected
                    clientSocket, address = self._serverSocket.accept()
                    inputSockets.append(clientSocket)

                    # Jython wants the socket to be non-blocking to run select()
                    clientSocket.setblocking(0)
                else:
                    data = sock.recv(bufferSize)
                    
                    if data:
                        # Data received
                        self._handleData(sock, data)
                    else:
                        # Client socket closed
                        sock.close()
                        inputSockets.remove(sock)

        self._serverSocket.close()
        print "Server finished"

    def stop(self):
        self._isListening = False

    def _handleData(self, socket, data):
        """Handle data received on a socket. As data may be received in chunks,
        these need to be assembled per-socket and then checked to see if a
        complete message has been received"""
        val = self._clientBuffers.get(socket, '')
        newVal = val + data

        messages = messageTokenizeRe.split(newVal)

        # Check whether there is a complete message...
        completeMsg = messageCompletenessRe.match(newVal) is not None
        if completeMsg:
            # Yes, wipe buffer
            self._clientBuffers[socket] = None
        else:
            # No, set buffer to partial message
            self._clientBuffers[socket] = messages.pop()

        for msg in messages:
            if msg != '':
                self.receiveMessage(msg)
