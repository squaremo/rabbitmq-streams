{
    "type": "feed",
    "wiring": {
	"nodes": {
	    "the_input1": {
	      "terminal": "socket_source_4567x"
	    },
	    "the_logger": {
	      "type": "logger"
	    },
	    "the_replacer": {
	      "type": "regexp_replace",
	      "configuration": {"expressions": [{"regexp": "(ra)bbit",
				                 "replacement": "\\1dical",
                                                 "multiline": false,
				                 "caseinsensitive": true,
				                 "dotall": false}]
			       }
	    },
	    "the_archiver": {
	      "terminal": "archive_destination"
	    },
	    "the_relay": {
	      "terminal": "relay"
            },
	    "the_socket_output": {
	      "terminal": "socket_destination_12345"
            }
	},
	"edges": [
          {"from": {"node": "the_input1"},
	   "to":   {"node": "the_logger", "channel": "input"}},
          {"from": {"node": "the_input1"},
	   "to":   {"node": "the_replacer", "channel": "input"}},
          {"from": {"node": "the_replacer", "channel": "output"},
	   "to":   {"node": "the_relay"}},
	  {"from": {"node": "the_relay"},
	   "to":   {"node": "the_archiver"}}
	]
    }
}
