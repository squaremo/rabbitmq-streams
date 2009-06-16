{
    "name": "XMPP XEP-0060 publisher gateway",
    "author": {"name": "Michael Bridgen", "email": "mikeb@lshift.net"},

    "type": "plugin-specification",
    "harness": "java",
    "subtype": "server",
    
    "global_configuration_specification": [],
    "configuration_specification": [
      {"label": "Host", "type": "hostname or IP address", "name": "host"},
      {"label": "Port", "type": "int", "name": "port"},
      {"label": "PubSub service", "type": "string", "name": "service"},
      {"label": "Username", "type": "string", "name": "username"},
      {"label": "Password", "type": "password", "name": "password"}
    ],

    "destination_specification": [{"label": "Node", "type": "string", "name": "node"}]
}
