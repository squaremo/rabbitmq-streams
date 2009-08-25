STREAMS_VERSION=0.1
FUNNY=
PACKAGE_FILE=rabbit-streams-${STREAMS_VERSION}.tar.gz
TOP_DIR=$(shell pwd)

all: clean rpms

prepare:
	mkdir -p BUILD SOURCES SPECS SRPMS RPMS tmp
	cat streams.spec | sed "s/^Version: \\(.*\\)/Version: $(STREAMS_VERSION)/" | \
	    (if [ -z "$(FUNNY)" ]; then \
		sed "s/^Revision: \\(.*\\)/Revision: 666/"; \
	    else cat; fi) \
          > SPECS/streams.spec
	cp -p -- "${PACKAGE_FILE}" SOURCES

rpms: prepare
	rpmbuild -ba --nodeps SPECS/streams.spec \
		--define '_topdir $(TOP_DIR)' \
		--define '_tmppath $(TOP_DIR)/tmp' \
		--target i386
	#rpmbuild -ba --nodeps SPECS/streams.spec \
		#--define '_topdir $(TOP_DIR)' \
		#--define '_tmppath $(TOP_DIR)/tmp' \
		#--target x86_64

clean:
	rm -rf SOURCES SPECS RPMS SRPMS BUILD tmp
