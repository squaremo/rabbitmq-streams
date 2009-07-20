>PLUGIN_INSTANCE_CONFIG	{"dotall": false, "regexp": "^1(.)3$", "multiline": false, "caseinsensitive": false, "replacement": "uno\\1tres"}
>input	123

<positive	uno2tres
>input	1
...	3

<positive	1
...	uno2tres
input	13

<negative	13
