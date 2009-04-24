{
    "name": "RSS Poller",
    "author": {"name": "Michael Bridgen", "email": "mikeb@lshift.net"},

    "type": "plugin",
    "harness": "python",
    "subtype": "pipeline_component",
    
    "global_configuration": [],
    "configuration_specification": [{"label": "URL", "type": "url", "name": "href"},
                      {"label": "Polling interval", "type": "int", "name": "interval"}],

    "inputs_specification": [],
    "outputs_specification": [{"name": "output"}],

    "database_specification": {}
}
