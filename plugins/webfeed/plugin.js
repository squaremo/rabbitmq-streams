{
    "name": "Web feed provider",
    "author": {"name": "Michael Bridgen", "email": "mikeb@lshift.net"},

    "type": "plugin",
    "harness": "python",
    "subtype": "pipeline_component",
    
    "global_configuration": [],
    "configuration": [
      {"label": "Title",
       "type": "string",
       "name": "title",
      }
    ],

    "inputs": {"input": {}},
    "outputs": {},
    "databases": {"content": {"views": ["newestfirst"]}}
}
