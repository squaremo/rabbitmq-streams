>PLUGIN_INSTANCE_CONFIG	{"expression": "//label/text()"}
>input	<label>Foobar</label>

<output	Foobar
# Nested
>input	<labels><label>Barfoo</label></labels>

<output	Barfoo
#
#Concatenate text nodes
>input	<labels><label>Baz</label><label>boo</label></labels>

<output	Baz
<output	boo
