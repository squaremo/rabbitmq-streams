>PLUGIN_INSTANCE_CONFIG	{"timeout": null, "count":7, "encoding": "utf-8-sep:.", "overlap":0, "unit": "B"}
>input	1
>input	2
>input	3
>input	uno

<output	1.2.3

>input	due

<output	uno.due

>input	quatre!

<output	quatre!

# exactly full
>input	1234567

<output	1234567
>input	123

>input	456

<output	123.456

#two messages will need to be published in one step
>input	1

>input	2345678

#FIXME(alexander) horrible hack
>SLEEP	1

<output	1
<output	2345678

# piecewise
>input	1

>input	2

>input	3

>input	4

<output	1.2.3.4
