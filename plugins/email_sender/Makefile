sources=$(shell find src -type f -name "*.java") pom.xml
jarfile=email_sender-1.0.jar

all: lib/$(jarfile)

clean:
	rm -rf target
	rm -f .libs-done
	rm -rf lib

target/$(jarfile): $(sources)
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
