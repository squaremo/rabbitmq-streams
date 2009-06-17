SOURCE_TARBALL_DIR=../../..
TOP_DIR=$(shell pwd)

rpms: clean feedshub

prepare:
	mkdir -p BUILD SOURCES SPECS SRPMS RPMS tmp
	cp feedshub.spec SPECS
	cp $(SOURCE_TARBALL_DIR)/FeedsHub-0.1.tar.gz SOURCES

feedshub: prepare
	rpmbuild -ba --nodeps SPECS/feedshub.spec \
		--define '_topdir $(TOP_DIR)' \
		--define '_tmppath $(TOP_DIR)/tmp' \
		--target i386
	rpmbuild -ba --nodeps SPECS/feedshub.spec \
		--define '_topdir $(TOP_DIR)' \
		--define '_tmppath $(TOP_DIR)/tmp' \
		--target x86_64
	
clean: 
	rm -rf SOURCES SPECS RPMS SRPMS BUILD tmp
