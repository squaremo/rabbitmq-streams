{
    "name": "Data sink",
    "author": {"name": "LShift Ltd."},

    "type": "plugin-specification",
    "harness": "python",
    "subtype": "pipeline_component",

    "global_configuration_specification": [],
    "configuration": [{"name": "endpointname",
		       "label": "Feed endpoint name",
		       "type": "identifier"},
		      {"name": "endpointtypes",
		       "label": "Endpoint types",
		       "type": "bitset",
		       "options": {"XMPP": "xmpp",
				   "Atom feed": "atom",
				   "Email": "email"}}],

    "inputs": [{"name": "input", "label": "Input"}],
    "outputs": [],

    "database": null
}
