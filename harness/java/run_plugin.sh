#!/bin/sh
#JVMARGS=-Xdebug -Xrunjdwp:transport=dt_socket,address=8998,server=y
JVMARGS=-Xms16m -Xmx16m
exec java $JVMARGS -jar feedshub_harness.jar $$ "$@"
