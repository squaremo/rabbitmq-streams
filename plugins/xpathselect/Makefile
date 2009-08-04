sources:=$(shell find src -type f -name "*.scala") pom.xml
jarfile=xpathselect-1.0-SNAPSHOT.jar
harness_jar=../../harness/java/feedshub_harness.jar

all: lib/$(jarfile)

lib/$(jarfile): target/$(jarfile)
	mkdir -p lib
	cp target/*.jar lib

target/$(jarfile): $(sources) $(harness_jar)
	mvn package

clean:
	mvn clean
	rm -rf lib
