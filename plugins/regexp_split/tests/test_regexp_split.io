>PLUGIN_INSTANCE_CONFIG	{"dotall": false, "regexp": "sausages", "multiline": false, "caseinsensitive": false}
>input	sausages

<positive	sausages

# something that w/o caseinsensitive shouldn't match
>input	SAUSAGES

<negative	SAUSAGES

>input	
...	
...	
...	

<negative	
...	
...	
...	

