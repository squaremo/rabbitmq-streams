#-*- encoding: utf-8 -*-
>PLUGIN_INSTANCE_CONFIG	{"expressions": [{"dotall": false, "regexp": "(.)\\1\\1", "multiline": false, "caseinsensitive": true, "replacement": "[3 x \"\\1\"]"}]}
>input	notnééÉgative

<output	notn[3 x "é"]gative
>input	pooositive

<output	p[3 x "o"]sitive
>input	really negative

<output	really negative
