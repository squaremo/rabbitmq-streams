CLASSPATH=
for i in lib/*.jar; do CLASSPATH=$i:$CLASSPATH; done
echo $CLASSPATH

java -cp "$CLASSPATH" org.thinkpond.jstest.Main test*.js
