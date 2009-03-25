all:
	@echo "Please choose a target from the makefile. (setup? build? clean? run?)"

setup: install-debs install-dev-debs install-couchdb

build:

clean:

veryclean: clean
	rm -rf build/opt/couchdb-0.9.0

absurdlyclean:
	rm -rf build

run:

install-couchdb: build/src/couchdb-0.9.0 build/opt/couchdb-0.9.0

build/src/couchdb-0.9.0:
	mkdir -p build/src
	svn co http://svn.apache.org/repos/asf/couchdb/tags/0.9.0/ build/src/couchdb-0.9.0

build/opt/couchdb-0.9.0:
	(cd build/src/couchdb-0.9.0; [ -f ./configure ] || ./bootstrap)
	(cd build/src/couchdb-0.9.0; ./configure --prefix="$(CURDIR)/build/opt/couchdb-0.9.0" && make && make install)

install-debs:
	sudo apt-get install daemontools-run ejabberd rabbitmq-server

install-dev-debs:
	: # these come from the couchdb README.
	sudo apt-get install automake autoconf libtool help2man
	sudo apt-get install build-essential erlang libicu38 libicu-dev libmozjs-dev libcurl4-openssl-dev

purge-debs:
	sudo apt-get purge ejabberd rabbitmq-server
