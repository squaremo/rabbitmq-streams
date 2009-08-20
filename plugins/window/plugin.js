{
    "name": "Window",
    "author": {"name": "Alexander Schmolck", "email": "alexander@lshift.net"},
    "type": "plugin-specification",
    "harness": "java",
    "subtype": "pipeline_component",

    "global_configuration_specification": [],
    "configuration_specification": [
      {"name": "timeout", "label": "timeout", "type": "float"},
      {"name": "unit", "label": "unit", "type": "string"},
      {"name": "count", "label": "count", "type": "number"},
      {"name": "overlap", "label": "overlap", "type": "int"},
      {"name": "encoding", "label": "encoding", "type": "string"}
    ],
    "inputs_specification": [{"name": "input", "label": "Input"}],
    "outputs_specification": [{"name": "output", "label": "Output"}],
    "database_specification": {}
}
