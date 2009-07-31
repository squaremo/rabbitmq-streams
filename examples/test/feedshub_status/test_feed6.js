{
    "type": "feed",
    "wiring": {
			"nodes": {
				"the_input": {
					"terminal": "socket_source_4567x"
	      },
				"the_output": {
					"terminal": "email_destination"
				},
				"a_logger": {
				    "type": "logger"
        }
			},
			"edges": [
	      {"from": {"node": "the_input"},
	       "to":   {"node": "a_logger", "channel": "input"}},
	      {"from": {"node": "the_input"},
	       "to": {"node": "the_output"}}
			]
    }
}
