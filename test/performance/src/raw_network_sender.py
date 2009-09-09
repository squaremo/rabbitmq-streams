from net.grinder.script.Grinder import grinder
from sender_grinder_util import SenderTestRunner
from sender import NetworkSender

HOST = grinder.properties.getProperty("streams.test.host")
PORT = int(grinder.properties.getProperty("streams.test.port"))

class TestRunner(SenderTestRunner):
    def __init__(self):
        netSender = NetworkSender((HOST, PORT))
        SenderTestRunner.__init__(self, netSender)
