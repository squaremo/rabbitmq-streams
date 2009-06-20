sources:=$(shell find src -type f -name "*.scala") pom.xml
jarfile=archive-1.0.jar
harness_jar=../../harness/java/feedshub_harness.jar

all: lib/$(jarfile)

clean:
	rm -rf target
	rm -rf lib

target/$(jarfile): $(sources) $(harness_jar)
	mvn package

lib/$(jarfile): target/$(jarfile)
	mkdir -p lib
	cp target/*.jar lib/
