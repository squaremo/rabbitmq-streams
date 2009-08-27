from net.grinder.script.Grinder import grinder
from sender_grinder_util import SenderTestRunner
from sender import HttpSender

URL = grinder.properties.getProperty("streams.test.url")

class TestRunner(SenderTestRunner):
    def __init__(self):
        # TODO: Variables
        httpSender = HttpSender(URL)
        SenderTestRunner.__init__(self, httpSender, 'test/performance/src/test-messages.txt')
