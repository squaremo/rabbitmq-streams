>PLUGIN_INSTANCE_CONFIG	{"dotall": false, "regexp": "(.)\\1\\1", "multiline": false, "caseinsensitive": false, "replacement": "[3 x \"\\1\"]"}
>input	pooositive

<positive	p[3 x "o"]sitive

# something that w/o caseinsensitive shouldn't match
>input	neeEgative

<negative	neeEgative

>input	
...	
...	
...	

<negative	
...	
...	
...	

