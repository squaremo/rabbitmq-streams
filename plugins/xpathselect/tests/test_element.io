>PLUGIN_INSTANCE_CONFIG	{"expression": "//label/text()"}
>input	<label>Foobar</label>

<output	Foobar
# Nested
>input	<labels><label>Barfoo</label></labels>

<output	Barfoo
#
#Multiple text nodes
>input	<labels><label>Baz</label><label>boo</label></labels>

<output	Baz
<output	boo
#
# Different nesting
>input	<labels><label>Foo</label><nest><label>Bar</label></nest></labels>

<output	Foo
<output	Bar
