{
    "type": "feed",
    "wiring": {
	"nodes": {
	    "input1": {
	      "terminal": "netlisten45678"
	    },
          "input2": {
            "terminal": "netlisten56789"
          },
	    "the_logger": {
	      "type": "logger"
	    },
	    "archive": {
	      "terminal": "toarchive"
	    },
	    "replacerabbit": {
	        "type": "regexp_replace",
		"configuration": {"regexp": "(ra)bbit",
				  "replacement": "\\1dical",
                                  "multiline": false,
				  "caseinsensitive": true,
				  "dotall": false
				 }
	    },
          "detectbadthings": {
	        "type": "regexp_split",
		"configuration": {"regexp": "bad things",
				  "replacement": "",
                                  "multiline": false,
				  "caseinsensitive": true,
				  "dotall": false
				 }            
          },
          "out1": {
	      "terminal": "netprint12345"
	    },
          "out2": {
	      "terminal": "netprint23456"
	    },
          "out3": {
	      "terminal": "netprint34567"
	    },
	    "relay": {
	      "terminal": "rabbitrelay"
            }
	},
	"edges": [
          {"from": {"node": "input1"},
	   "to":   {"node": "the_logger", "channel": "input"}},
          {"from": {"node": "input1"},
	   "to":   {"node": "replacerabbit", "channel": "input"}},
          {"from": {"node": "replacerabbit", "channel": "negative"},
	   "to":   {"node": "out2"}},
          {"from": {"node": "replacerabbit", "channel": "positive"},
	   "to":   {"node": "out3"}},
          {"from": {"node": "input2"},
           "to": {"node": "out3"}},
          {"from": {"node": "replacerabbit", "channel": "positive"}, "to": {"node": "archive"}}
	]
    }
}
