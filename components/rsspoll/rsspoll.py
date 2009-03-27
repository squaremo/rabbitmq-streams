from feedshub import Source, cons_db, config_of_db, db_from_config, ensure_db
from fetcher import fetch

class RssPollerSource(Source):
    
    def run(self):
        import time
        state = self.getState()
        contentdb = None
        try:
            # TODO factor this into Component.ensureDatabase() or similar
            if not state.has_key('content'):
                contentserver = self.setting('contentserver')
                contentdb = cons_db(contentserver)
                state['content'] = config_of_db(contentdb)
            else:
                ensure_db(state['content'])
                contentdb = db_from_config(state['content'])
        except Exception, e:
            msg = "Could not find or create content database"
            self.error(msg)
            raise msg, e

        if not state.has_key('href'):
            state['href'] = self.setting('href')
        href = state['href']

        pollinterval = self.setting('pollinterval')
        lastpolled = state.get('lastpolled', 0)
        self.putState(state)

        def modified(state):
            return ('modified' in state) and time.gmtime(state['modified']) or None

        def poll(state):
            response = fetch(contentdb, href, dict(etag=state.get('etag', None),
                                                   modified=modified(state)))
            if 'error' in response:
                self.error(response['error'])
            
            if 'updated' in response:
                # TODO notify of new items
                for msg in response['updated']:
                    self.publish(msg)
    
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
                self.putState(state)
                lastpolled = state['lastpolled']
                time.sleep(pollinterval)
            else:
                time.sleep(pollAt - now)


def run(config):
    component = RssPollerSource(config)
    component.start()
