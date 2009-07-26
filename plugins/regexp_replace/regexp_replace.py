from feedshub import Component
from feedshub import json
import re

LOCALFLAGS = ('multiline', 'caseinsensitive', 'dotall')
GLOBALFLAGS = ('global_multiline', 'global_caseinsensitive', 'global_dotall')

def makeFlags(config, defaults=(False, False, False), keys=LOCALFLAGS):
    keyAndDefault = zip(keys, defaults)
    multiline, caseinsensitive, dotall = \
        tuple(config.get(*kd) for kd in keyAndDefault)
    f = 0
    if multiline:
        f |= re.MULTILINE
    if caseinsensitive:
        f |= re.IGNORECASE
    if dotall:
        f |= re.DOTALL
    return f

class RegexpReplacer(Component):

    def __init__(self, config):
        super(RegexpReplacer, self).__init__(config)

    def matcher(self, expression, globalFlags):
        match = expression["regexp"]
        flags = makeFlags(expression, defaults=globalFlags)
        return re.compile(match, flags)
        
    def input(self, msg, config):
        globalFlags = [config.get(k, False) for k in GLOBALFLAGS]
        body = msg.body
        for expression in config["expressions"]:
            match = self.matcher(expression, globalFlags)
            replacement = expression["replacement"]
            result, _ = re.subn(match, replacement, body)
            body = result
        self.output(body)

def run(config):
    rr = RegexpReplacer(config)
    rr.start()
