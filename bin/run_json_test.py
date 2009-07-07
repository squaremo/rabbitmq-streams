#!/usr/bin/python


json = r'''
 [
  { "description": "Test empty lines",
    "configuration": {"regexp": "^",
                      "replacement": "Discard: ",
                      "multiline": false, "caseinsensitive": false, "dotall": false
                     },
    "expect" : [
      {"in": "out" :},
    ]
  }
  { "description": "Test caseinsensitive",
    "configuration": {"regexp": "^",
                      "replacement": "Discard: ",
                      "multiline": false, "caseinsensitive": true, "dotall": false
                        },
    "expect" : [
      {"in": "out" :}
    ]
  }

]

'''
