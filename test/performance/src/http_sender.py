from net.grinder.script.Grinder import grinder
from sender_grinder_util import SenderTestRunner
from sender import HttpSender

URL = grinder.properties.getProperty("streams.test.url")

class TestRunner(SenderTestRunner):
    def __init__(self):
        httpSender = HttpSender(URL)
        SenderTestRunner.__init__(self, httpSender)
