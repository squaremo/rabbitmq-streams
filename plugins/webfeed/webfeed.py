from feedshub import Component
import sha

import simplejson as json

class FeedArchiver(Component):
    INPUTS = {'input': 'accept'}

    def accept(self, msg):
        body = msg.body
        id = sha.new(body).hexdigest()
        db = self.privateDatabase()
        if id not in db:
            db[id] = {'entry': body}
        self.ack(msg)

    def run(self):
        super(FeedArchiver, self).run()

def run(config):
    archiver = FeedArchiver(config)
    archiver.start()
