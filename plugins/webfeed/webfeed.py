from feedshub import Component

class FeedArchiver(Component):
    INPUTS = {'input': 'accept'}

    def accept(self, msg):
        pass

    def run(self):
        super(FeedArchiver, self).run()

def run(config):
    archiver = FeedArchiver(config)
    archiver.start()
