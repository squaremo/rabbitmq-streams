{
    "name": "Data sink",
    "author": {"name": "LShift Ltd."},

    "type": "plugin",
    "harness": "python",
    "subtype": "pipeline_component",

    "global_configuration": [],
    "configuration": [{"name": "endpointname",
		       "label": "Feed endpoint name",
		       "type": "identifier"},
		      {"name": "endpointtypes",
		       "label": "Endpoint types",
		       "type": "bitset",
		       "options": {"XMPP": "xmpp",
				   "Atom feed": "atom",
				   "Email": "email"}}],

    "inputs": [{"name": "input"}],
    "outputs": []
}
