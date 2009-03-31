from feedshub import Component

class FeedArchiver(Component):
    INPUTS = {'input': 'accept'}

    def accept(self, msg):
        id = msg['_id']
        db = self.privateDatabase()
        if id not in db:
            db[id] = msg

    def run(self):
        super(FeedArchiver, self).run()

def run(config):
    archiver = FeedArchiver(config)
    archiver.start()
