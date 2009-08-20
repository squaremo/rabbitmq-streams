# -*- encoding: utf-8 -*-
>PLUGIN_INSTANCE_CONFIG	{"timeout": null, "count":9, "encoding": "utf-8-sep:|||", "overlap":0, "unit": "B"}
>input	123
# one utf-8 character but three bytes
>input	三

<output	123|||三
