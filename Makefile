all:
	@echo "Please choose a target from the makefile. (setup? update? build? clean? run?)"

setup: install-debs install-dev-debs create-build-logs-dir install-couchdb install-rabbitmq

update: update-rabbitmq

update-rabbitmq: build/src/rabbitmq-codegen build/src/rabbitmq-server
	rm -rf build/scratch build/opt/rabbitmq
	(cd build/src/rabbitmq-codegen && hg pull && hg update)
	(cd build/src/rabbitmq-server && hg pull && hg update)
	$(MAKE) build/opt/rabbitmq

build:

clean:

veryclean: clean
	rm -rf build/opt
	rm -rf build/scratch
	-[ -d build/src/couchdb-0.9.0 ] && $(MAKE) -C build/src/couchdb-0.9.0 clean
	-[ -d build/src/rabbitmq-server ] && $(MAKE) -C build/src/rabbitmq-server clean

absurdlyclean:
	rm -rf build

run:

create-build-logs-dir:
	mkdir -p build/logs

install-couchdb: build/src/couchdb-0.9.0 build/opt/couchdb-0.9.0

install-rabbitmq: build/src/rabbitmq-codegen build/src/rabbitmq-server build/opt/rabbitmq

build/src/rabbitmq-codegen:
	@echo Cloning rabbitmq-codegen ...
	(mkdir -p build/src && cd build/src && hg clone http://hg.rabbitmq.com/rabbitmq-codegen) \
		> build/logs/clone-rabbitmq-codegen.txt 2>&1

build/src/rabbitmq-server:
	@echo Cloning rabbitmq-server ...
	(mkdir -p build/src && cd build/src && hg clone http://hg.rabbitmq.com/rabbitmq-server) \
		> build/logs/clone-rabbitmq-server.txt 2>&1

build/opt/rabbitmq:
	@echo Building rabbitmq-server ...
	(cd build/src/rabbitmq-server && $(MAKE) srcdist) \
		> build/logs/build-rabbitmq-server.txt 2>&1
	mkdir -p build/scratch \
		>> build/logs/build-rabbitmq-server.txt 2>&1
	(cd build/scratch && tar -zxvf ../src/rabbitmq-server/dist/rabbitmq-server-0.0.0.tar.gz) \
		>> build/logs/build-rabbitmq-server.txt 2>&1
	(cd build/scratch/rabbitmq-server-0.0.0 && $(MAKE) install PYTHON=python2.5 TARGET_DIR="$(CURDIR)/build/opt/rabbitmq" SBIN_DIR="$(CURDIR)/build/opt/rabbitmq/sbin" MAN_DIR="$(CURDIR)/build/opt/rabbitmq/man") \
		>> build/logs/build-rabbitmq-server.txt 2>&1

build/src/couchdb-0.9.0:
	@echo Checking out CouchDB from svn ...
	mkdir -p build/src && svn co http://svn.apache.org/repos/asf/couchdb/tags/0.9.0/ build/src/couchdb-0.9.0 \
		> build/logs/checkout-couchdb.txt 2>&1

build/opt/couchdb-0.9.0:
	@echo Building CouchDB ...
	(cd build/src/couchdb-0.9.0; [ -f ./configure ] || ./bootstrap) \
		> build/logs/build-couchdb.txt 2>&1
	(cd build/src/couchdb-0.9.0; ./configure --prefix="$(CURDIR)/build/opt/couchdb-0.9.0" && $(MAKE) && $(MAKE) install) \
		>> build/logs/build-couchdb.txt 2>&1

install-debs:
	: # none at the minute.

install-dev-debs:
	: # these come from the couchdb README.
	sudo apt-get install automake autoconf libtool help2man
	sudo apt-get install build-essential erlang libicu38 libicu-dev libmozjs-dev libcurl4-openssl-dev
