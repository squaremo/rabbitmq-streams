{
    "type": "feed",
    "active": true,
    "user": "testuser",
    "wiring": {
	"nodes": {
	    "the_input": {
		"type": "relay_in"
	    },
	    "the_transformer": {
		"type": "xslt"
	    },
	    "the_output": {
		"type": "relay_out"
	    }
	},
	"edges": [
	    ["the_input", "output", "the_transformer", "input"],
	    ["the_transformer", "output", "the_output", "input"]
	]
    }
}
