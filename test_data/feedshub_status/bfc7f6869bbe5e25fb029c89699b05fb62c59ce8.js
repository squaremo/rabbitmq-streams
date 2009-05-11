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
	    },
	    "the_relay": {
	      "terminal": "d4bcd34acd92a9cad22b394de6b599f538685be2"
            },
	    "the_socket_output": {
	      "terminal": "09f99bf874bd489fad3323254e709371dbd56987"
            }
	},
	"edges": [
          {"from": {"node": "the_input1"},
	   "to":   {"node": "the_logger", "channel": "input"}},
          {"from": {"node": "the_input1"},
	   "to":   {"node": "the_replacer", "channel": "input"}},
          {"from": {"node": "the_replacer", "channel": "positive"},
	   "to":   {"node": "the_relay"}},
          {"from": {"node": "the_replacer", "channel": "negative"},
	   "to":   {"node": "the_socket_output"}},
	  {"from": {"node": "the_relay"},
	   "to":   {"node": "the_archiver"}}
	]
    }
}
