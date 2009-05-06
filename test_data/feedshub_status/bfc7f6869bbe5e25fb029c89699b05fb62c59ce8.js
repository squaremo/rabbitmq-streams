{
    "type": "feed",
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
          {"from": {"node": "the_input1", "channel": "output"},
	   "to":   {"node": "the_output", "channel": "input"}},
          {"from": {"node": "the_input1", "channel": "output"},
	   "to":   {"node": "the_output", "channel": "input"}}
	]
    }
}
