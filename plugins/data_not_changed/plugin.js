{
    "name": "Data not changed",
    "author": {"name": "Alexander Schmolck", "email": "alexander@lshift.net"},

    "type": "plugin-specification",
    "harness": "python",
    "subtype": "pipeline_component",

    "global_configuration_specification": [],
    "configuration_specification": [
      {"label": "message",
       "type": "string",
       "name": "message"
      }
    ],

    "inputs_specification": [{"name": "input", "label": "Input"}],
    "outputs_specification": [{"name": "nag"}],
    "database_specification": {}
}
