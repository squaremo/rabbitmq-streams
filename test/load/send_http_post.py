from net.grinder.script.Grinder import grinder
from net.grinder.script import Test
from net.grinder.plugin.http import HTTPRequest
from HTTPClient import NVPair

from jarray import zeros
from java.lang import System

URL = grinder.properties.getProperty("streams.test.url")

MSG_SIZE = grinder.properties.getInt("streams.test.msg_size", 128)

log = grinder.logger.output

class TestRunner:
    # TODO: Some comments and explanation

    def __call__(self):
        test = Test(1, "HTTP post")
        request = test.wrap(HTTPRequest())

        timestamp = "|timestamp=" + str(System.currentTimeMillis()) + "|"
        padding = 'X' * (MSG_SIZE - len(timestamp))
        data = timestamp + padding + '\n'
        
        result = request.POST(URL, data)

        if not result.statusCode == 204:
            raise Exception("Unexpected HTTP response; " + result.getText())

        
