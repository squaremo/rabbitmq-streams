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
	    "detect_iplayer_or_ylt": {
	        "type": "regexp_split",
	      "configuration": {"regexp": "(^iplayer.*)|(^you're listening to.*)",
                                  "multiline": false,
				  "caseinsensitive": true,
				  "dotall": false
				 }
	    },
	    "replace_goonline": {
	      "type": "regexp_replace",
	      "configuration": {"expressions": [{"regexp": "(g)o online at",
				                 "replacement": "\\1o to",
                                                 "multiline": false,
				                 "caseinsensitive": true,
				                 "dotall": false}]
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
	   "to":   {"node": "detect_iplayer_or_ylt", "channel": "input"}},

          {"from": {"node": "detect_iplayer_or_ylt", "channel": "negative"},
	   "to":   {"node": "replace_goonline", "channel": "input"}},
	  {"from": {"node": "detect_iplayer_or_ylt", "channel": "positive"},
	   "to":   {"node": "discard"}},

	  {"from": {"node": "replace_goonline", "channel": "output"},
	   "to":   {"node": "broadcast"}}
	]
    }
}
