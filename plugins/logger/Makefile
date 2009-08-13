sources:=$(shell find src -type f -name "*.java") pom.xml
jarfile=logger-1.0.jar
harness_jar=../../harness/java/feedshub_harness.jar

all: lib/$(jarfile)

target/$(jarfile): $(sources) $(harness_jar)
ifeq ($(TEST),false)
	mvn package -Dmaven.test.skip=true
else
	mvn package
endif

lib/$(jarfile): target/$(jarfile)
	mkdir -p lib
	cp target/*.jar lib

clean:
	mvn clean
	rm -rf lib
