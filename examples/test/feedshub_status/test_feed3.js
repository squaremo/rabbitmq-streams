{
    "type": "feed",
    "wiring": {
	"nodes": {
	    "the_input": {
		"terminal": "websubscriber_source"
            },
	    "the_output": {
		"terminal": "archive_destination"
	    },
	    "a_logger": {
	        "type": "logger"
            }
	},
	"edges": [
          {"from": {"node": "the_input"},
           "to":   {"node": "a_logger", "channel": "input"}},
          {"from": {"node": "the_input"},
           "to":   {"node": "the_output"}}
	]
    }
}
