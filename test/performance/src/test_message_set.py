import re
import unittest

import message_set

class testMessageSet(unittest.TestCase):

    def setUp(self):
        self._ms = message_set.MessageSet('test-messages.txt')

    def testDefaultRe(self):
        self.assertEquals(
            '<label network="TEST_NETWORK">Test message #1</label>\n',
            self._ms.next())
        
    def testNext(self):
        regEx = re.compile('<label network=".*">(.*)</label>')
        ms = message_set.MessageSet('test-messages.txt', regEx)
        
        self.assertEquals('Test message #1', ms.next())
        self.assertEquals('Test message #2', ms.next())
        self.assertEquals('Test message #3', ms.next())
        self.assertEquals('Test message #1', ms.next())        

if __name__ == '__main__':
    unittest.main()
