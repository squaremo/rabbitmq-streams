>PLUGIN_INSTANCE_CONFIG	{"expressions": [{"regexp":"foo", "replacement":"bar"}, {"regexp":"barf","replacement":"burp"}]}
>input	foo

<output	bar
>input	foo boo

<output	bar baz

>input	foof

<output	burp
