JAVA_PLUGINS=$(shell find . -name pom.xml)
PLUGIN_DIRS=$(foreach plugin, $(JAVA_PLUGINS), $(shell dirname $(plugin)))

all: mvn 
	$(foreach dir, $(PLUGIN_DIRS), $(shell mkdir -p $(dir)/lib; cp $(dir)/target/*.jar $(dir)/lib))

mvn:
	mvn install

clean:
	mvn clean
	$(foreach dir, $(PLUGIN_DIRS), $(shell rm -rf $(dir)/lib))

