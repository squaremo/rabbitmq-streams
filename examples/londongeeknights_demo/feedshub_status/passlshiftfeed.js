{
    "type": "feed",
    "wiring": {
	"nodes": {
	    "input1": {
		"terminal": "rabbitrelay"
	    },
	    "correctlshift": {
	        "type": "regexp_replace",
		"configuration": {"regexp": "[lL][Ss]hift",
				  "replacement": "LShift",
                                  "multiline": false,
				  "caseinsensitive": true,
				  "dotall": false
				 }
	    },
	    "print": {
		"terminal": "netprint23456"
	    }
	},
	"edges": [
            {"from": {"node": "input1"},
             "to":   {"node": "correctlshift", "channel": "input"}},

            {"from": {"node": "correctlshift", "channel": "positive"},
             "to":   {"node": "print"}},
            {"from": {"node": "correctlshift", "channel": "negative"},
             "to":   {"node": "print"}}
	]
    }
}
