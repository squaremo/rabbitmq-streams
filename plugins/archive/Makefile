sources:=$(shell find src -type f -name "*.scala") pom.xml
jarfile=archive-1.0.jar
harness_jar=../../harness/java/feedshub_harness.jar

all: lib/$(jarfile)

clean:
	rm -rf target
	rm -rf lib
	rm -rf config/feedshub_status

target/$(jarfile): $(sources) $(harness_jar)
ifeq ($(TEST),false)
	mvn package -Dmaven.test.skip=true
else
	mvn package
endif

lib/$(jarfile): target/$(jarfile)
	mkdir -p lib
	cp target/*.jar lib/

build-config:
	mkdir -p config/feedshub_status
	../../bin/compile.sh config/feedshub_status config/simple.js
