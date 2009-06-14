sources=$(shell find src -type f -name "*.scala") pom.xml
jarfile=archive-1.0.jar
harness_jar=../../harness/java/feedshub_harness.jar

all: lib/$(jarfile)

clean:
	rm -rf target
	rm -f .libs-done
	rm -rf lib

.libs-done: $(harness_jar)
	mvn install:install-file -DgroupId=net.lshift.feedshub -DartifactId=feedshub-harness -Dversion=1.0 -Dpackaging=jar -Dfile=$(harness_jar)
	touch .libs-done

target/$(jarfile): $(sources) .libs-done
	mvn package
# Mavan may decide that nothing needs to be done, even
# if say, feedshub_harness has a newer timestamp.  So we
# make sure that if this target ran, it will look like
# the jar file was updated.
	touch target/$(jarfile)

lib/$(jarfile): target/$(jarfile)
	mkdir -p lib
	cp target/*.jar lib/
