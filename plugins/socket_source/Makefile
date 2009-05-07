all: socket_source.class

clean:
	rm -f *.class

socket_source.class: socket_source.java ../../harness/java/feedshub_harness.jar
	CLASSPATH=../../harness/java/feedshub_harness.jar javac socket_source.java
