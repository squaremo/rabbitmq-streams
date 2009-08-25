#!/bin/sh
# E.g.s,
# cat sample | simulate-load.sh "curl -d @- http://localhost:8888/livetext" 0.1
# grep radio1 sample | simulate-load.sh "nc localhost 45678" 1
while read line; do echo "Sending [$1]: $line"; echo "$line" | $1 ; sleep $2; done
