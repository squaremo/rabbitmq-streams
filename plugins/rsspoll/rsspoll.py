from feedshub import Component
from fetch import fetch
import sys

class RssPollerSource(Component):
    
    OUTPUTS = {'output': 'publish'}

    def error(self, msg):
        raise Exception(str(msg))

    def run(self):
        import time
        state = self.getState()
        contentdb = self.privateDatabase()

        if 'href' not in state:
            state['href'] = self.setting('href')
        href = state['href']

        pollinterval = self.setting('interval')
        lastpolled = state.get('lastpolled', 0)
        state = self.putState(state)

        def modified(state):
            return ('modified' in state) and time.gmtime(state['modified']) or None

        def poll(state):
            response = fetch(contentdb, href, dict(etag=state.get('etag', None),
                                                   modified=modified(state)))
            if 'error' in response:
                self.error(str(response['error']))

            if 'updated' in response:
                # TODO notify of new items
                updated = response['updated']
                for msg in updated:
                    self.publish(msg['entry'], content_type='application/atom+xml')
                self.commit()
                contentdb.update(updated)

            state.update(response)
            return state

        while True:
            now = time.time()
            pollAt = lastpolled + pollinterval

            # There are a few behaviours that we could have here.  We could
            # try to keep to the 'original' schedule, by sleeping again if we
            # wake up early, and sleeping a bit less if we were woken up late.
            # Or, we could trust that the scheduler will on average wake us up
            # at the right time.
            #
            # We do a mixture: we sleep a bit more if we wake up early, but
            # don't try to sleep less if we wake up late.  The rationale is
            # that if we're waking up late, it's probably because the system
            # is under load, and waking more often can only make things worse.
            if pollAt < now:
                state = poll(state)
                state = self.putState(state)
                lastpolled = state['lastpolled']
                time.sleep(pollinterval)
            else:
                time.sleep(pollAt - now)

def run(config):
    component = RssPollerSource(config)
    component.start()
