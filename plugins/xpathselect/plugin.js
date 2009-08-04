{
    "name": "XPath evaluator",
    "author": {"name": "Michael Bridgen", "email": "mikeb@lshift.net"},
    "help": "Evaluates the given XPath expression against the incoming content, and sends on each of the resulting DOM nodes separately.",
    "type": "plugin-specification",
    "harness": "java",
    "subtype": "pipeline_component",
    
    "global_configuration_specification": [],
    "configuration_specification": [
      {"name": "expression",
       "label": "XPath expression",
       "type": "string"
      }
    ],

    "inputs_specification": [{"name": "input", "label": "Input"}],
    "outputs_specification": [{"name": "output", "label": "Output"}]
}
