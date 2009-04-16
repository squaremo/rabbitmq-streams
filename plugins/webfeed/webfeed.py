from feedshub import Component
import sha

import simplejson as json

class FeedArchiver(Component):
    INPUTS = {'input': 'accept'}

    def accept(self, msg):
        id = sha.new(msg).hexdigest()
        db = self.privateDatabase()
        if id not in db:
            db[id] = {'entry': msg}

    def run(self):
        super(FeedArchiver, self).run()

def run(config):
    archiver = FeedArchiver(config)
    archiver.start()
