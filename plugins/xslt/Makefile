all: xslt.class

clean:
	rm -f *.class

xslt.class: xslt.java ../../harness/java/feedshub_harness.jar
	CLASSPATH=../../harness/java/feedshub_harness.jar javac xslt.java
