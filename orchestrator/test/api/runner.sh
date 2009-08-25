#!/bin/bash

CLASSPATH=
for i in lib/*.jar; do CLASSPATH=$i:$CLASSPATH; done
#echo $CLASSPATH

NODENAME=testing

mkdir -p tmp
TMP=`mktemp -d tmp/streamsXXXXX`
DB=`basename $TMP | sed -e 's/\.//' |tr "[:upper:]" "[:lower:]"`
TARGET=`dirname $TMP`/$DB
[[ "$TARGET" = "/$DB" ]] && (echo "Managed to target /"; exit 1)

DBURL=http://localhost:5984/$DB
curl -s -X PUT $DBURL >/dev/null

STREAMS_LOG_BASE=`(cd tmp; pwd)` \
STREAMS_NODENAME=$NODENAME \
STREAMS_CONFIG_DB=$DB \
../../scripts/streams-server 2>&1 > tmp/server.out &

TESTS=$1
[[ "x" = "x$TESTS" ]] && TESTS=test*.js

for t in $TESTS; do
    echo Compiling fixture to $TARGET
    rm -rf $TARGET
    mkdir -p $TARGET
    ../../../bin/compile.sh $TARGET stub.js $t setup.js
    curl -s -X DELETE $DBURL >/dev/null
    curl -s -X PUT $DBURL >/dev/null
    python ../../../sbin/import_config.py -f $TARGET
    ../../scripts/streamsctl -n $NODENAME restart
    java -cp "$CLASSPATH" org.thinkpond.jstest.Main $t
done

[[ "x$NOTIDY" = "x" ]] && (echo "Cleaning up .."; curl -s -X DELETE $DBURL >/dev/null; rm -rf tmp)

../../scripts/streamsctl -n $NODENAME stop

