#-*- encoding: utf-8 -*-
>PLUGIN_INSTANCE_CONFIG	{"dotall": false, "regexp": "^.ausages$", "multiline": false, "caseinsensitive": false}
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
>input	三ausages

<positive	三ausages

>input	SSausages

<negative	SSausages

