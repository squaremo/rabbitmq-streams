all: libs
	mvn package
	mkdir -p lib
	cp target/*.jar lib/

clean:
	rm -r target

libs:
	mvn install:install-file -DgroupId=net.lshift.feedshub -DartifactId=feedshub-harness -Dversion=1.0 -Dpackaging=jar -Dfile=../../harness/java/feedshub_harness.jar
