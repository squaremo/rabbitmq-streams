sources=$(shell find src -type f -name "*.scala") pom.xml
jarfile=xmpppubsub-1.0.jar

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
