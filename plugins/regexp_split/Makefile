all: 
	mvn install
	mkdir -p dist
	cp target/*.jar dist
	cp -rf dist lib # This should just be until rpm packaging fixed

clean:
	mvn clean
	rm -rf dist
