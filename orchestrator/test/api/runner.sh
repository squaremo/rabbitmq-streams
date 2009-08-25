#!/bin/bash

CLASSPATH=
for i in lib/*.jar; do CLASSPATH=$i:$CLASSPATH; done
#echo $CLASSPATH

NODENAME=testing
TMP=`mktemp -d tmp/streamsXXXXX`
DB=`basename $TMP | sed -e 's/\.//' |tr "[:upper:]" "[:lower:]"`
TARGET=`dirname $TMP`/$DB
[[ "$TARGET" = "/$DB" ]] && exit

DBURL=http://localhost:5984/$DB
curl -X PUT $DBURL

STREAMS_NODENAME=$NODENAME STREAMS_CONFIG_DB=$DB ../../scripts/streams-server 2>&1 > tmp/server.out &

TESTS=$1
[[ "x" = "x$TESTS" ]] && TESTS=test*.js

for t in $TESTS; do
    echo Compiling fixture to $TARGET
    rm -rf $TARGET
    mkdir -p $TARGET
    ../../../bin/compile.sh $TARGET stub.js $t setup.js
    curl -X DELETE $DBURL
    curl -X PUT $DBURL
    python ../../../sbin/import_config.py -f $TARGET
    ../../scripts/streamsctl -n $NODENAME restart
    java -cp "$CLASSPATH" org.thinkpond.jstest.Main $t
done

curl -X DELETE $DBURL
../../scripts/streamsctl -n $NODENAME stop
