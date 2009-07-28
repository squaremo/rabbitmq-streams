>PLUGIN_INSTANCE_CONFIG	{"expressions": [{"regexp":"foo", "replacement":"bar"}, {"regexp":"bar","replacement":"baz"}]}
# The replacements are applied to the output of the previous
>input	foo

<output	baz
# .. and in order
>input	bar

<output	baz
