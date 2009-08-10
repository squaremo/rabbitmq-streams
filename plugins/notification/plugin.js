{
    "name": "Notification gateway",
    "author": {"name": "Michael Bridgen", "email": "mikeb@lshift.net"},

    "type": "plugin-specification",
    "harness": "java",
    "subtype": "server",
    
    "global_configuration_specification": [],
    "configuration_specification": [
      {"label": "Host", "name": "host", "type": "hostname"},
      {"label": "Port", "name": "port", "type": "integer < 65535"},
      {"label": "Virtual host", "name": "virtual_host", "type": "string"},
      {"label": "Username", "name": "username", "type": "string"},
      {"label": "Password", "name": "password", "type": "string"}
    ],

    "source_specification": [{"label": "Key", "type": "string", "name": "bindingkey"}]
}
