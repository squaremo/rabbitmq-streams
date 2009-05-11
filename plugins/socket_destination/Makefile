all: socket_destination.class

clean:
	rm -f *.class

socket_destination.class: socket_destination.java ../../harness/java/feedshub_harness.jar
	CLASSPATH=../../harness/java/feedshub_harness.jar javac socket_destination.java
