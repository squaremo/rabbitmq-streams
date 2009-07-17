from net.grinder.script.Grinder import grinder
from net.grinder.script import Test
from net.grinder.plugin.http import HTTPRequest
from HTTPClient import NVPair

from jarray import zeros
from java.lang import System

# TODO: Make script parameter
URL = "http://localhost:9876/foo"

# The size of the message to send
MSG_SIZE = 1024


log = grinder.logger.output

class TestRunner:
    # TODO: Some comments and explanation

    def __call__(self):
        test = Test(1, "HTTP post")
        request = test.wrap(HTTPRequest())

        # TODO: Add random data generation

        timestamp = "|timestamp=" + str(System.currentTimeMillis()) + "|"
        padding = 'X' * (MSG_SIZE - len(timestamp))
        data = timestamp + padding + '\n'
        
        result = request.POST(URL, data)

        if not result.statusCode == 204:
            raise Exception("Unexpected HTTP response; " + result.getText())

        
