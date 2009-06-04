all: feedshub_harness.jar

feedshub_harness.jar:
	mvn install
	cp plugin/target/plugin-1.0-SNAPSHOT.jar feedshub_harness.jar

clean:
	mvn clean
	rm -f feedshub_harness.jar
