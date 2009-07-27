{
    "type": "feed",
    "wiring": {
	"nodes": {
	    "the_input": {
		"type": "websubscriber",
		"configuration": {"href": "http://www.lshift.net/blog/feed",
                                  "interval": 30}
	    },
	    "the_transformer": {
		"type": "xslt",
		"configuration": {"stylesheet_url": "http://dev.lshift.net/matthew/sample.xslt"}
	    },
	    "the_regexp": {
		"type": "regexp_split",
		"configuration": {"regexp": "^.*Rabbit.*$",
				  "multiline": true,
				  "caseinsensitive": false,
				  "dotall": true
			         }
	    },
	    "the_replacer": {
	        "type": "regexp_replace",
	      "configuration": {"expressions": [{"regexp": "(ra)bbit",
				                 "replacement": "\\1dical",
                                                 "multiline": false,
				                 "caseinsensitive": true,
				                 "dotall": false
				                }]}
	    },
	    "the_output_pos": {
		"type": "webfeed",
                "configuration": {"title": "Test xslt with Rabbit"}
	    },
	    "the_output_neg": {
		"type": "webfeed",
                "configuration": {"title": "Test xslt without Rabbit"}
	    },
	    "a_logger": {
	        "type": "logger"
            }
	},
	"edges": [
            {"from": {"node": "the_input", "channel": "output"},
             "to":   {"node": "the_transformer", "channel": "input"}},

            {"from": {"node": "the_transformer", "channel": "output"},
             "to":   {"node": "the_regexp", "channel": "input"}},

            {"from": {"node": "the_regexp", "channel": "positive"},
             "to":   {"node": "the_replacer", "channel": "input"}},

            {"from": {"node": "the_regexp", "channel": "negative"},
             "to":   {"node": "the_output_neg", "channel": "input"}},

            {"from": {"node": "the_replacer", "channel": "output"},
             "to":   {"node": "the_output_pos", "channel": "input"}},

            {"from": {"node": "the_replacer", "channel": "output"},
             "to":   {"node": "a_logger", "channel": "input"}}
	]
    }
}
