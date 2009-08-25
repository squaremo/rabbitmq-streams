import re

class MessageSet:
    """Load in a set of messages from the given file.
    Each message must be delimited by a line break, and the supplied regular
    expression can be used to extract the content from the line."""
    def __init__(self, filename, extractRE = None):
        rawMessages = None
        
        with open(filename, 'r') as f:
            rawMessages = f.readlines()

        if(extractRE == None):
            self._messages = rawMessages
        else:
            self._messages = self._extractMessages(rawMessages, extractRE)
            
        self._iter = self._messages.__iter__()

    def next(self):
        """Return the next message in the set. If all messages have been
        returned, then start back at the beginning"""
        try:
            return self._iter.next()
        except StopIteration:
            self._iter = self._messages.__iter__()
            return self._iter.next()

    def _extractMessages(self, rawMessages, extractRE):
        return map(lambda msg: extractRE.match(msg).group(1),  rawMessages)
