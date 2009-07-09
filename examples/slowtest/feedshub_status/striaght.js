{
    "type": "feed",
    "wiring": {
      "nodes": {
	"the_input1": {
	  "terminal": "netlisten45678"
	},
	"the_output1": {
            "terminal": "netprint12345"
        },
	"slowtestplugin": {
	        "type": "slowtestplugin"
	}
      },
      "edges": [
	{"from": {"node": "the_input1"},
	 "to":   {"node": "slowtestplugin", "channel": "cache"}},
	{"from": {"node": "slowtestplugin", "channel": "output"},
	 "to":   {"node": "the_output1"}}
      ]
    }
}
