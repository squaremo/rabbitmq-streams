all: logger.class

clean:
	rm -f *.class

logger.class: logger.java ../../harness/java/feedshub_harness.jar
	CLASSPATH=../../harness/java/feedshub_harness.jar javac logger.java
