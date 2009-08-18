CLASSPATH=
for i in lib/*.jar; do CLASSPATH=$i:$CLASSPATH; done
echo $CLASSPATH

for t in test*.js; do
    TARGET=`mktemp -d`
    echo Compiling fixture to $TARGET
    ../../../bin/compile.sh $TARGET stub.js $t setup.js
    java -cp "$CLASSPATH" org.thinkpond.jstest.Main $t
done
