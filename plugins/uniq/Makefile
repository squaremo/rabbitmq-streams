sources=$(shell find src -type f -name "*.scala") pom.xml
jarfile=uniq-1.0.jar

all: lib/$(jarfile)

clean:
	rm -rf target
	rm -f .libs-done
	rm -rf lib

target/$(jarfile): $(sources)
	mvn package

lib/$(jarfile): target/$(jarfile)
	mkdir -p lib
	cp target/*.jar lib/
