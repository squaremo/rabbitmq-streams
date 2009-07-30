{
    "name": "Data Timeout",
    "author": {"name": "Alexander Schmolck", "email": "alexander@lshift.net"},

    "type": "plugin-specification",
    "harness": "python",
    "subtype": "pipeline_component",

    "global_configuration_specification": [],
    "configuration_specification": [
      {"label": "Timeout interval",
       "type": "float",
       "name": "timeout"
      },
      {"label": "Timeout message",
       "type": "string",
       "name": "timeout_message"
      }
    ],

    "inputs_specification": [{"name": "input", "label": "Input"}],
    "outputs_specification": [],

    "database_specification": {}

}
