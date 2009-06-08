SOURCE_TARBALL_DIR=../../dist
TOP_DIR=$(shell pwd)

prepare:
	mkdir -p BUILD SOURCES SPECS SRPMS RPMS tmp
	cp feedshub.spec SPECS

rpms: clean feedshub

feedshub: prepare
	rpmbuild -bp --nodeps SPECS/feedshub.spec \
		--define '_topdir $(TOP_DIR)' \
		--define '_tmppath $(TOP_DIR)/tmp'
	
clean: 
	rm -rf SOURCES SPECS RPMS SRPMS BUILD tmp
