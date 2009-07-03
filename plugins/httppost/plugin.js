{
    "name": "HTTP POST receiver",
    "author": {"name": "Tony Garnock-Jones", "email": "tonyg@lshift.net"},

    "type": "plugin-specification",
    "harness": "java",
    "subtype": "server",
    
    "global_configuration_specification": [],
    "configuration_specification": [{"name": "http_server_host",
				     "label": "HTTP server host name or IP address",
				     "type": "hostname or IP address"},
				    {"name": "http_server_port",
				     "label": "HTTP server port number",
				     "type": "natural < 65536"}],

    "source_specification": [{"name": "url_path",
			      "label": "URL path for this POST endpoint",
			      "type": "string"}]
}
