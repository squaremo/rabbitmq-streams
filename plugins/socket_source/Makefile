all: 
	mvn install
	mkdir -p dist
	cp target/*.jar dist
	cp -rf dist lib

clean:
	mvn clean
	rm -rf dist
