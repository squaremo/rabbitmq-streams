import re
import sys
import threading
import time

import simplejson as json

from feedshub import Component, NO_DATA

class DataTimeout(Component):
    """Logs if `DataTimeout.input` hasn't been written to in ``timeout`` secs.

    For robustness (in case the plugin dies), the timeout state is persisted.
    """
    def __init__(self, config):
        super(DataTimeout, self).__init__(config)
        self.timeout = self.setting('timeout')
        self.timeout_message = self.setting('timeout_message')
        now = time.time()
        # NB: we assume that the operations below have negligible latency
        # compared to `self.timeout`; this isn't necessarily true and can mean
        # that after silence for ``N*self.timeout`` secs, less than ``N``
        # timeout messages get send. I don't see this as a practical problem
        # though. -- AS
        state = self.getState({'alarm': now + self.timeout})
        if state['alarm'] <= now:
            self.notify(NO_DATA, self.timeout_message)
            state['alarm'] = now + self.timeout
        self.putState(state)
        self.reset_timer(state['alarm'] - now)

    def reset_timer(self, timeout):
        if hasattr(self, 'timer'): self.timer.cancel()
        self.timer = threading.Timer(timeout, self.notify, [NO_DATA, self.timeout_message])
        self.timer.start()

    def input(self, stuff, _):
        self.reset_timer(self.timeout)

def run(config):
    rr = DataTimeout(config)
    rr.start()
