{
    "name": "Web feed provider",
    "author": {"name": "Michael Bridgen", "email": "mikeb@lshift.net"},

    "type": "plugin",
    "harness": "python",
    "subtype": "pipeline_component",
    
    "global_configuration": [],
    "configuration": [
      {"label": "Title",
       "type": "string",
       "name": "title"
      }
    ],

    "inputs": [{"name": "input", "label": "Input"}],
    "outputs": [],

    "database": {
      "_design/articles": {
        "language": "javascript",
        "views": {
          "newestfirst": {
            "map": "function (doc) { /* do something */ }"
          }
        }
      }
    }
}
