CLASSPATH=
for i in lib/*.jar; do CLASSPATH=$i:$CLASSPATH; done
echo $CLASSPATH

for t in test*.js; do
    TMP=`mktemp -d`
    DB=`basename $TMP | sed -e 's/\.//' |tr "[:upper:]" "[:lower:]"`
    TARGET=`dirname $TMP`/$DB
    mv $TMP $TARGET
    echo Compiling fixture to $TARGET
    ../../../bin/compile.sh $TARGET stub.js $t setup.js
    ../../../sbin/import_config.py -f $TARGET
    
    java -cp "$CLASSPATH" org.thinkpond.jstest.Main $t
done
