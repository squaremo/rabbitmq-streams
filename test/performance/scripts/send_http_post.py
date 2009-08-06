from net.grinder.script.Grinder import grinder
from net.grinder.script import Test
from net.grinder.plugin.http import HTTPRequest
from HTTPClient import NVPair

from jarray import zeros
from java.lang import System

URL = grinder.properties.getProperty("streams.test.url")

MSG_SIZE = grinder.properties.getInt("streams.test.msg_size", 128)

# The target message rate per second. The implementation of this is crude and
# this figure will actually be a maximum
MSG_RATE = grinder.properties.getDouble("streams.test.msg_rate", 0)

log = grinder.logger.output

log("URL: %s" % URL)
log("Message size: %i bytes" % MSG_SIZE)
log("Message rate: %i tps" % MSG_RATE)

sleepTime = 0
if MSG_RATE > 0:
    sleepTimeSec = 1 / MSG_RATE
    sleepTime = sleepTimeSec * 1000

class TestRunner:
    # TODO: Some comments and explanation

    def __call__(self):
        t1 = System.currentTimeMillis()

        test = Test(1, "HTTP post")
        request = test.wrap(HTTPRequest())

        timestamp = "|timestamp=" + str(System.currentTimeMillis()) + "|"
        padding = 'X' * (MSG_SIZE - len(timestamp))
        data = timestamp + padding + '\n'

        result = request.POST(URL, data)

        if not result.statusCode == 204:
            raise Exception("Unexpected HTTP response; " + result.getText())

        if sleepTime > 0:
            # Adjust sleep time based on test time
            adjustedSleeptime = sleepTime - (System.currentTimeMillis() - t1)
            grinder.sleep(long(adjustedSleeptime))
