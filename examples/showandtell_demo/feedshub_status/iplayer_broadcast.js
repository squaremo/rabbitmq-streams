{
    "type": "feed",
    "wiring": {
	"nodes": {
	    "the_input1": {
	      "terminal": "iplayer_broadcast_relay"
	    },
	    "archiver": {
	      "terminal": "archive_broadcast"
            },
          "print": {
            "terminal": "netprint12345"
          }
	},
	"edges": [
	  {"from": {"node": "the_input1"},
	   "to":   {"node": "archiver"}},
	  {"from": {"node": "the_input1"},
	   "to":   {"node": "print"}}
	]
    }
}
