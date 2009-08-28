>PLUGIN_INSTANCE_CONFIG	{"expressions": [{"regexp":"foo", "replacement":"bar"}, {"regexp":"barf","replacement":"burp"}]}
>input	foo

<output	bar
>input	foo boo

<output	bar boo

>input	foof

<output	burp
