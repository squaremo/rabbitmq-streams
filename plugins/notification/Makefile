sources=$(shell find src -type f -name "*.scala") pom.xml
jarfile=notification-1.0-SNAPSHOT.jar
harness_jar=../../harness/java/feedshub_harness.jar

all: lib/$(jarfile)

clean:
	rm -rf target
	rm -rf lib

target/$(jarfile): $(sources) $(harness_jar)
ifeq ($(TEST),false)
	mvn package -Dmaven.test.skip=true
else
	mvn package
endif

lib/$(jarfile): target/$(jarfile)
	mkdir -p lib
	cp target/*.jar lib/
