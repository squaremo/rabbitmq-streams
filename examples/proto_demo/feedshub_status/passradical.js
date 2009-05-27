{
    "type": "feed",
    "wiring": {
	"nodes": {
	    "the_input1": {
	      "terminal": "radicalrelay"
	    },
	    "the_logger": {
	      "type": "logger"
	    },
	    "the_printer": {
	      "terminal": "netprint12345"
            }
	},
	"edges": [
          {"from": {"node": "the_input1"},
	   "to":   {"node": "the_logger", "channel": "input"}},
	  {"from": {"node": "the_input1"},
	   "to":   {"node": "the_printer"}}
	]
    }
}
