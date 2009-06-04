{
    "type": "feed",
    "wiring": {
	"nodes": {
	    "the_input1": {
	      "terminal": "netlisten45678"
	    },
	    "the_logger": {
	      "type": "logger"
	    },
	    "detect_iplayer": {
	        "type": "regexp_split",
	      "configuration": {"regexp": "iplayer",
				  "replacement": "",
                                  "multiline": false,
				  "caseinsensitive": true,
				  "dotall": false
				 }
	    },
	    "detect_ylt": {
	        "type": "regexp_split",
		"configuration": {"regexp": "you're listening to",
				  "replacement": "",
                                  "multiline": false,
				  "caseinsensitive": true,
				  "dotall": false
				 }
	    },
	    "replace_goonline": {
	        "type": "regexp_replace",
	      "configuration": {"regexp": "(g)o online at",
				  "replacement": "\\1o to",
                                  "multiline": false,
				  "caseinsensitive": true,
				  "dotall": false
				 }
	    },
	    "discard": {
	      "terminal": "relay_discard"
            },
	    "broadcast": {
	      "terminal": "iplayer_broadcast_relay"
            }
        },
	"edges": [
          {"from": {"node": "the_input1"},
	   "to":   {"node": "the_logger", "channel": "input"}},
          {"from": {"node": "the_input1"},
	   "to":   {"node": "detect_iplayer", "channel": "input"}},
          {"from": {"node": "detect_iplayer", "channel": "negative"},
	   "to":   {"node": "detect_ylt", "channel": "input"}},
          {"from": {"node": "detect_ylt", "channel": "negative"},
	   "to":   {"node": "replace_goonline", "channel": "input"}},
	  {"from": {"node": "replace_goonline", "channel": "positive"},
	   "to":   {"node": "broadcast"}},
	  {"from": {"node": "replace_goonline", "channel": "negative"},
	   "to":   {"node": "broadcast"}},

          {"from": {"node": "detect_iplayer", "channel": "positive"},
	   "to":   {"node": "discard"}},
          {"from": {"node": "detect_iplayer", "channel": "positive"},
	   "to":   {"node": "discard"}}
	]
    }
}
