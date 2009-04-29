from feedshub import Component
import simplejson as json
import re

class RegexpReplacer(Component):

    def __init__(self, config):
        super(RegexpReplacer, self).__init__(config)
        match = self.setting("regexp")
        replacement = self.setting("replacement")

        multiline = self.setting("multiline")
        caseinsensitive = self.setting("caseinsensitive")
        dotall = self.setting("dotall")

        flags = 0
        if multiline:
            flags |= re.MULTILINE
        if caseinsensitive:
            flags |= re.IGNORECASE
        if dotall:
            flags |= re.DOTALL
        self.__regexp = re.compile(match, flags)
        self.__replacement = replacement
        self.info(match)


    def input(self, msg):
        body = msg.body
        result, count = re.subn(self.__regexp, self.__replacement, body)
        if 0 == count:
            self.negative(body)
        else:
            self.positive(result)

def run(config):
    rr = RegexpReplacer(config)
    rr.start()

    def run(self):
        super(FeedArchiver, self).run()
