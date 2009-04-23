CLASSPATH := lib/couchdb4j-read-only/dist/couchdb4j-0.3.0-SVN.jar:lib/couchdb4j-read-only/lib/commons-beanutils.jar:lib/couchdb4j-read-only/lib/commons-codec-1.3.jar:lib/couchdb4j-read-only/lib/commons-collections.jar:lib/couchdb4j-read-only/lib/commons-lang.jar:lib/couchdb4j-read-only/lib/commons-logging-1.1.jar:lib/couchdb4j-read-only/lib/ezmorph-1.0.3.jar:lib/couchdb4j-read-only/lib/httpclient-4.0-beta2.jar:lib/couchdb4j-read-only/lib/httpcore-4.0-beta2.jar:lib/couchdb4j-read-only/lib/json-lib-2.2.3-MODIFIED-jdk15.jar:lib/rabbitmq-java-client-1.5.4/build/lib/rabbitmq-client.jar

SOURCE := $(shell find src -type f -name "*.java")

harness-jar: java-harness
	./build_harness_jar.sh

java-harness:
	- mkdir bin
	javac -classpath $(CLASSPATH) -d bin $(SOURCE)

clean:
	rm -rf ./bin
	rm -f feedshub_harness.jar
