SOURCE_TARBALL_DIR=../../..
TOP_DIR=$(shell pwd)

all: clean rpms

prepare:
	mkdir -p BUILD SOURCES SPECS SRPMS RPMS tmp
	cp streams.spec SPECS/
	cp $(SOURCE_TARBALL_DIR)/Streams-0.1.tar.gz SOURCES

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
