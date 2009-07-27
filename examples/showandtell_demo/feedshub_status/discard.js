{
    "type": "feed",
    "wiring": {
	"nodes": {
	    "the_input1": {
	      "terminal": "relay_discard"
	    },
	    "archiver": {
	      "terminal": "archiveother"
            },
          "prepend": {
	    "type": "regexp_replace",
	    "configuration": {"expressions": [{"regexp": "^",
			                       "replacement": "Discard: ",
                                               "multiline": false,
			                       "caseinsensitive": true,
			                       "dotall": false}]
			     }
          }
	},
	"edges": [
	  {"from": {"node": "the_input1"},
	   "to":   {"node": "prepend", "channel": "input"}},
          {"from": {"node": "prepend", "channel": "output"},
	   "to":   {"node": "archiver"}}
	]
    }
}
