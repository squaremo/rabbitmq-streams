import feedshub as fh

class FeedArchiver(fh.Sink):
    def accept(msg):
        pass

def run(config):
    archiver = FeedArchiver(config)
    archiver.start()

