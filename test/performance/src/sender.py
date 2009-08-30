from net.grinder.plugin.http import HTTPRequest

# TODO: Remove duplication
def consoleLog(msg):
    print msg

class Sender:
    """Parent class of all senders"""
    def __init__(self, logFunction = consoleLog):
        self._log = logFunction

    def log(self, msg):
        self._log(msg)

    def send(self, data):
        pass

class HttpSender(Sender):
    def __init__(self, url, logFunction = None):
        self._url = url
        Sender.__init__(self, logFunction)

    def send(self, data):
        request = HTTPRequest()
        result = request.POST(self._url, data)

        if not result.statusCode == 204:
            raise Exception("Unexpected HTTP response; " + result.getText())

        return result
