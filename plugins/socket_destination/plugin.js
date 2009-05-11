{
    "name": "Socket Destination",
    "author": {"name": "Matthew Sackman", "email": "matthew@lshift.net"},

    "type": "plugin-specification",
    "harness": "java",
    "subtype": "server",
    
    "global_configuration_specification": [],
    "configuration_specification": [],

    "destination_specification": [{"label": "Port", "type": "natural < 65536", "name": "port"},
                                  {"label": "Host", "type": "hostname or IP address", "name": "host"}
                                 ]
}
