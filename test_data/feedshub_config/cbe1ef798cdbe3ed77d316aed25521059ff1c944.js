{
    "type": "feed",
    "active": true,
    "user": "testuser",
    "wiring": {
	"nodes": {
	    "the_input": {
		"type": "rsspoll",
		"configuration": {"href": "http://www.lshift.net/blog/feed",
                                  "interval": 30}
	    },
	    "the_transformer": {
		"type": "xslt",
		"configuration": {"stylesheet_url": "http://dev.lshift.net/matthew/sample.xslt"}
	    },
	    "the_output": {
		"type": "webfeed",
                "configuration": {"title": "Test xslt"}
	    }
	},
	"edges": [
	    ["the_input", "output", "the_transformer", "input"],
	    ["the_transformer", "output", "the_output", "input"]
	]
    }
}
