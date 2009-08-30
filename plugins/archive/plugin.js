{
  "name": "archive",
	"author": { "name" : "LShift Ltd." },
	"type": "plugin-specification",
  "harness": "java",
  "subtype": "server",
  "global_configuration_specification": [],
  "configuration_specification": [
    {"name": "host", "label": "Host", "type": "String"},
    {"name": "port", "label": "Port", "type": "natural < 65536"},
    {"name": "username", "label": "Username", "type": "String"},
    {"name": "password", "label": "Password", "type": "String"}
  ],
  "destination_specification": [ { "name": "name", "label": "Archive name", "type": "String" } ],
  "database_specification": {}
}
