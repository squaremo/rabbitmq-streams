sources:=$(shell find . -name "*.java")
poms:=$(shell find . -name "pom.xml")

all: feedshub_harness.jar

feedshub_harness.jar: $(sources) $(poms)
	mvn install
	cp plugin/target/plugin-1.0-SNAPSHOT.jar feedshub_harness.jar

clean:
	mvn clean
	rm -f feedshub_harness.jar
