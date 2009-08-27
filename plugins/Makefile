JAVA_PLUGINS=$(shell find . -mindepth 2 -name pom.xml)
PLUGIN_DIRS=$(foreach plugin, $(JAVA_PLUGINS), $(shell dirname $(plugin)))
PLUGIN_DIR_TARGETS=$(foreach dir, $(PLUGIN_DIRS), ${dir}/target)

all: mvn
	$(foreach dir, $(PLUGIN_DIRS), $(shell mkdir -p $(dir)/lib; cp $(dir)/target/*.jar $(dir)/lib))

mvn: $(PLUGIN_DIR_TARGETS)

%/target:
	(cd $(shell dirname $@); mvn package)

clean:
	mvn clean
	$(foreach dir, $(PLUGIN_DIRS), $(shell rm -rf $(dir)/lib))

