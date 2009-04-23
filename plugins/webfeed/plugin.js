{
    "name": "Web feed provider",
    "author": {"name": "Michael Bridgen", "email": "mikeb@lshift.net"},

    "type": "plugin",
    "harness": "python",
    "subtype": "pipeline_component",
    
    "global_configuration": [],
    "configuration_specification": [
      {"label": "Title",
       "type": "string",
       "name": "title"
      }
    ],

    "inputs_specification": [{"name": "input", "label": "Input"}],
    "outputs_specification": [],

    "database_specification": {
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
