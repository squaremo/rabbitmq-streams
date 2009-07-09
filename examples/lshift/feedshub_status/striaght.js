{
    "type": "feed",
    "wiring": {
      "nodes": {
	"the_input1": {
	  "terminal": "netlisten45678"
	},
	"the_cache_input": {
	  "terminal": "netlisten45679"
	},
	"the_output1": {
            "terminal": "netprint12345"
        },
	"onevaluecache": {
	        "type": "onevaluecache"
	},
	"headeroutput": {
	        "type": "headeroutput"
	}
      },
      "edges": [
	{"from": {"node": "the_input1"},
	 "to":   {"node": "onevaluecache", "channel": "input"}},
	{"from": {"node": "the_input1"},
	 "to":   {"node": "onevaluecache", "channel": "cache"}},
	{"from": {"node": "onevaluecache", "channel": "output"},
	 "to":   {"node": "the_output1"}}
      ]
    }
}
