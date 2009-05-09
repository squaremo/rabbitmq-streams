{
    "name": "Regular Expression Splitter",
    "author": {"name": "Matthew Sackman", "email": "matthew@lshift.net"},

    "type": "plugin-specification",
    "harness": "java",
    "subtype": "pipeline_component",
    
    "global_configuration_specification": [],
    "configuration_specification": [
      {"label": "Regexp",
       "type": "string",
       "name": "regexp"
      },
      {"label": "Multiline",
       "type": "boolean",
       "name": "multiline"
      },
      {"label": "Case insensitive",
       "type": "boolean",
       "name": "caseinsensitive"
      },
      {"label": "Dot all",
       "type": "boolean",
       "name": "dotall"
      }
    ],

    "inputs_specification": [{"name": "input", "label": "Input"}],
    "outputs_specification": [{"name": "positive", "label": "Positive"},
			      {"name": "negative", "label": "Negative"}
			     ],

    "database_specification": null
}
