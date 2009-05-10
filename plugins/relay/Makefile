all: relay.class

clean:
	rm -f *.class

relay.class: relay.java ../../harness/java/feedshub_harness.jar
	CLASSPATH=../../harness/java/feedshub_harness.jar javac relay.java
