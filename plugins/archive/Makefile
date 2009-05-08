sources=$(shell find src -type f -name "*.scala") pom.xml
jarfile=lib/archive-1.0.jar

all: libs $(jarfile)

clean:
	rm -r target
	rm .libs-done

libs: .libs-done

.libs-done:
	mvn install:install-file -DgroupId=net.lshift.feedshub -DartifactId=feedshub-harness -Dversion=1.0 -Dpackaging=jar -Dfile=../../harness/java/feedshub_harness.jar
	touch .libs-done

$(jarfile): $(sources)
	mvn package
	mkdir -p lib
	cp target/*.jar lib/
