{
    "name": "RSS Poller",
    "author": {"name": "Michael Bridgen", "email": "mikeb@lshift.net"},

    "type": "plugin-specification",
    "harness": "python",
    "subtype": "pipeline_component",
    
    "global_configuration_specification": [],
    "configuration_specification": [{"label": "URL", "type": "url", "name": "href"},
                      {"label": "Polling interval", "type": "int", "name": "interval"}],

    "inputs_specification": [],
    "outputs_specification": [{"name": "output", "label": "Output"}],

    "database_specification": null
}
