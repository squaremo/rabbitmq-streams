sources=$(shell find src -type f -name "*.scala") pom.xml
jarfile=archive-1.0.jar
harness_jar=../../harness/java/feedshub_harness.jar

all: libs
	mvn install

clean:
	mvn clean

lib-clean:
	rm .libs-done

libs: .libs-done

.libs-done: $(harness_jar)
	mvn install:install-file -DgroupId=net.lshift.feedshub -DartifactId=feedshub-harness -Dversion=1.0 -Dpackaging=jar -Dfile=$(harness_jar)
	mvn install:install-file -DgroupId=jcouchdb -DartifactId=jcouchdb -Dversion=0.9.0-2 -Dpackaging=jar -Dfile=lib/jcouchdb-0.9.0-2.jar
	mvn install:install-file -DgroupId=net.svenson -DartifactId=svenson -Dversion=1.3.0 -Dpackaging=jar -Dfile=lib/svenson-1.3.0.jar
	mvn install:install-file -DgroupId=net.rabbitmq -DartifactId=amqp-client -Dversion=1.5.4 -Dpackaging=jar -Dfile=lib/rabbitmq-client-1.5.4.jar
	touch .libs-done

run: libs
	mvn jetty:run
