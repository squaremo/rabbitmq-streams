while read line; do echo "Sending [$1:$2]: $line"; nc $1 $2 < $line; sleep $3; done
