sources=$(shell find src -type f -name "*.scala") pom.xml
jarfile=site-1.0-SNAPSHOT.war
harness_jar=../../harness/java/feedshub_harness.jar

all: target/$(jarfile)

target/$(jarfile): $(sources) .libs-done
	mvn package

clean:
	mvn clean

lib-clean:
	rm .libs-done

.libs-done: $(harness_jar)
	mvn install:install-file -DgroupId=net.lshift.feedshub -DartifactId=feedshub-harness -Dversion=1.0 -Dpackaging=jar -Dfile=$(harness_jar)
	touch .libs-done

run: all
	mvn jetty:run
