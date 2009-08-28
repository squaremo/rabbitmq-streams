#!/bin/sh
WD="`dirname $0`/compiler"
java -jar "$WD/js.jar" -f "$WD/ast.js" -f "$WD/json2.js" "$WD/compiler.js" "$@"
