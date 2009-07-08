{
    "type": "feed",
    "wiring": {
	"nodes": {
	    "the_input": {
		"terminal": "socket_source_4567x"
            },
	    "the_output": {
		"terminal": "socket_destination_12345"
	    },
          "js": {
            "type": "javascript",
            "configuration": {
              "function": "function(msg) {return msg.toUpperCase();}"
            }
          },
	    "a_logger": {
	        "type": "logger"
            }
	},
	"edges": [
          {"from": {"node": "the_input"},
           "to":   {"node": "a_logger", "channel": "input"}},
          {"from": {"node": "the_input"},
           "to": {"node": "js", "channel": "input"}},
          {"from": {"node": "js", "channel": "output"},
           "to":   {"node": "the_output"}}
	]
    }
}
