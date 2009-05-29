sources=$(shell find src -type f -name "*.scala") pom.xml
jarfile=archive-1.0.jar
harness_jar=../../harness/java/feedshub_harness.jar
all: libs lib/$(jarfile)

clean:
	rm -rf target
	rm -f .libs-done
	rm -rf lib

libs: .libs-done

.libs-done: $(harness_jar)
	mvn install:install-file -DgroupId=net.lshift.feedshub -DartifactId=feedshub-harness -Dversion=1.0 -Dpackaging=jar -Dfile=$(harness_jar)
	touch .libs-done

target/$(jarfile): $(sources)
	mvn package

lib/$(jarfile): target/$(jarfile)
	mkdir -p lib
	cp target/*.jar lib/
