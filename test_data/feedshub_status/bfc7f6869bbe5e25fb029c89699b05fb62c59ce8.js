{
    "type": "feed",
    "wiring": {
	"nodes": {
	    "the_input1": {
	      "terminal": "fbc1a57519d773628a67701036d0e3f6c6459a38"
	    },
	    "the_logger": {
	      "type": "logger"
	    },
	    "the_replacer": {
	        "type": "regexp_replace",
		"configuration": {"regexp": "(ra)bbit",
				  "replacement": "\\1dical",
                                  "multiline": false,
				  "caseinsensitive": true,
				  "dotall": false
				 }
	    },
	    "the_archiver": {
	      "terminal": "7d2a9f159071e1c6f6edaccb59ae5f9c80688b6c"
	    }
	},
	"edges": [
          {"from": {"node": "the_input1"},
	   "to":   {"node": "the_logger", "channel": "input"}},
          {"from": {"node": "the_input1"},
	   "to":   {"node": "the_replacer", "channel": "input"}},
          {"from": {"node": "the_replacer", "channel": "positive"},
	   "to":   {"node": "the_archiver"}}
	]
    }
}
