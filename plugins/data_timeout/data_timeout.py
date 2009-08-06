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
        self.state = self.getState(None)
        if self.state is None:
            now = time.time()
            self.state = {'alarm': now + self.timeout}
            self.putState(self.state)
        if not self.maybe_trigger_timer():
            self.reset_timer()

    def maybe_trigger_timer(self):
        now = time.time()
        if self.state['alarm'] <= now:
            self.notify(NO_DATA, self.timeout_message)
            self.reset_timer()
            return True
        return False

    def reset_timer(self):
        # NB: we assume that the operations below have negligible latency
        # compared to `self.timeout`; this isn't necessarily true and can mean
        # that after silence for ``N*self.timeout`` secs, less than ``N``
        # timeout messages get send. I don't see this as a practical problem
        # though. -- AS
        if hasattr(self, 'timer'): self.timer.cancel()
        now = time.time()
        self.state = self.getState()
        self.state['alarm'] = now + self.timeout
        self.putState(self.state)
        self.timer = threading.Timer(self.timeout, self.maybe_trigger_timer, [])
        self.timer.start()

    def input(self, stuff, _):
        self.reset_timer()

def run(config):
    rr = DataTimeout(config)
    rr.start()
