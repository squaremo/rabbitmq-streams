{
    "name": "Data Timeout",
    "author": {"name": "Michael Bridgen", "email": "mikeb@lshift.net"},
    "help": "A timer; this will send a NoData notification when it's not seen data for timeout milliseconds.",

    "type": "plugin-specification",
    "harness": "java",
    "subtype": "pipeline_component",

    "global_configuration_specification": [],
    "configuration_specification": [
      {"label": "Timeout interval (ms)",
       "type": "long",
       "name": "timeout"
      },
      {"label": "Timeout message",
       "type": "string",
       "name": "timeout_message"
      }
    ],

    "inputs_specification": [{"name": "input", "label": "Input"}],
    "outputs_specification": [{"name": "output", "label": "Pass-through"}]
}
