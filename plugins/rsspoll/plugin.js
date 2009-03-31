{
    "name": "RSS Poller",
    "author": {"name": "Michael Bridgen", "email": "mikeb@lshift.net"},

    "type": "plugin",
    "harness": "python",
    "subtype": "pipeline_component",
    
    "global_configuration": [],
    "configuration": [{"label": "URL", "type": "url", "name": "href"}
                      {"label": "Polling interval", "type": "int", "name": "interval"}],

    "inputs": [],
    "outputs": [{"name": "output"}],

    "database": {}
}
