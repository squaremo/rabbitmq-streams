{
    "name": "Javascript evaluator",
    "author": {"name": "Michael Bridgen", "email": "mikeb@lshift.net"},

    "type": "plugin-specification",
    "harness": "java",
    "subtype": "pipeline_component",
    
    "global_configuration_specification": [],
    "configuration_specification": [
      {"name": "function",
       "label": "Transformation function",
       "type": "string"
      }
    ],

    "inputs_specification": [{"name": "input", "label": "Input"}],
    "outputs_specification": [{"name": "output", "label": "Output"}],
    "database_specification": null
}
