#!/bin/sh

BASEDIR="`dirname $0`/.."
PLUGINDIR="$BASEDIR/plugins"

for jsfile in $(find "$PLUGINDIR" -name plugin.js)
do
    echo $jsfile ...
    cat $jsfile | $BASEDIR/bin/schemacheck.js $BASEDIR/bin/plugin.schema.js Plugin
done
