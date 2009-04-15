{
    "type": "feed",
    "active": true,
    "user": "testuser",
    "wiring": {
	"nodes": {
	    "the_input": {
		"type": "rsspoll",
		"configuration": {"href": "http://www.lshift.net/blog/feed",
                                  "interval": 120}
	    },
	    "the_transformer": {
		"type": "xslt"
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
