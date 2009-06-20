from feedshub import Plugin
import simplejson as json
import re

class RegexpReplacer(Plugin):

    def __init__(self, config):
        super(RegexpReplacer, self).__init__(config)
        
    def input(self, msg, config):
        match = config["regexp"]
        replacement = config["replacement"]
        multiline = config["multiline"]
        caseinsensitive = config["caseinsensitive"]
        dotall = config["dotall"]

        flags = 0
        if multiline:
            flags |= re.MULTILINE
        if caseinsensitive:
            flags |= re.IGNORECASE
        if dotall:
            flags |= re.DOTALL
        regexp = re.compile(match, flags)
        body = msg.body
        result, count = re.subn(regexp, replacement, body)
        if 0 == count:
            self.negative(body)
        else:
            self.positive(result)

def run(config):
    rr = RegexpReplacer(config)
    rr.start()
