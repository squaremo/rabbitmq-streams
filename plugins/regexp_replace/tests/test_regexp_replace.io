#-*- encoding: utf-8 -*-
>PLUGIN_INSTANCE_CONFIG	{"expressions": [{"regexp": "(.)\\1\\1", "replacement": "[3 x \"$1\"]"}]}
>input	pooositive

<output	p[3 x "o"]sitive

# something that w/o caseinsensitive shouldn't match
>input	neeEgative

<output	neeEgative

>input	
...	
...	
...	

<output	
...	
...	
...	
>input	三三三

<output	[3 x "三"]

