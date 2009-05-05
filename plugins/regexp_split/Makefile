all: regexp_split.class

clean:
	rm -f *.class

regexp_split.class: regexp_split.java ../../harness/java/feedshub_harness.jar
	CLASSPATH=../../harness/java/feedshub_harness.jar javac regexp_split.java
