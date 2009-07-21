{
    "type": "feed",
    "wiring": {
        "nodes": {
            "the_input1": {
                "terminal": "httppost_a"
            },
            "the_printer": {
                "terminal": "netprint12345"
            }
        },
        "edges": [
                  {
                "from": {
                    "node": "the_input1"
                },
                "to":   {
                    "node": "the_printer"
                }
            }]

    }
}
