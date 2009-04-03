{
    "type": "feed",
    "active": true,
    "user": "testuser",
    "wiring": {
	"nodes": {
	    "the_input1": {
	      "type": "rsspoll",
              "configuration": {"href": "http://www.lshift.net/blog/feed",
                                "interval": 120}
	    },
	    "the_input2": {
	      "type": "rsspoll",
              "configuration": {"href": "http://wellquite.org/rss/wellquite.xml",
                                "interval": 120}
	    },
	    "the_output": {
	      "type": "webfeed",
              "configuration": {"title": "LShift blog again"}
	    }
	},
	"edges": [
	  ["the_input1", "output", "the_output", "input"],
          ["the_input2", "output", "the_output", "input"]
	]
    }
}
