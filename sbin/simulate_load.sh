while read line; do echo "Sending [$1:$2]: $line"; echo "$line" | nc $1 $2 ; sleep $3; done
