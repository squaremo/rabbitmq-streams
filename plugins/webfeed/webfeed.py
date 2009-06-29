from feedshub import Component
import sha

try:
    import simplejson as json
except ImportError:
    import json

class FeedArchiver(Component):
    INPUTS = {'input': 'accept'}

    def accept(self, msg):
        body = msg.body
        id = sha.new(body).hexdigest()
        db = self.privateDatabase()
        if id not in db:
            db[id] = {'entry': body}

    def run(self):
        super(FeedArchiver, self).run()

def run(config):
    archiver = FeedArchiver(config)
    archiver.start()
