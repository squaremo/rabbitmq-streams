>PLUGIN_INSTANCE_CONFIG	{"timeout_message": "game over", "timeout": 10}
>SLEEP	11s
<notify	game over
# Now stave off the timeout
>input	immediate

>SLEEP	3s
>input	also immediate

# Should be reset
>SLEEP	10s
<notify	game over
# Reset; and another
>SLEEP	10s
<notify	game over
