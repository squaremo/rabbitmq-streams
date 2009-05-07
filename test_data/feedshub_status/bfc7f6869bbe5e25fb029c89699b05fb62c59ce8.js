{
    "type": "feed",
    "wiring": {
	"nodes": {
	    "the_input1": {
	      "terminal": "fbc1a57519d773628a67701036d0e3f6c6459a38"
	    },
	    "the_output": {
	      "type": "logger"
	    }
	},
	"edges": [
          {"from": {"node": "the_input1"},
	   "to":   {"node": "the_output", "channel": "input"}}
	]
    }
}
