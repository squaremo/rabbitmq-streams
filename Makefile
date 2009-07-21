SHELL=/bin/bash
PYTHON=python -Wignore::DeprecationWarning
RABBITMQCTL=./build/opt/rabbitmq/sbin/rabbitmqctl -q
RABBITSTREAMS_CONF=etc/rabbitstreams.conf

CONFIG_DOC=$$(cat $(RABBITSTREAMS_CONF) | bin/json --raw get config_doc)
COUCH_SERVER=$$(echo $(CONFIG_DOC) | egrep -o 'http://[^/]+')

# !!!: this needs to evaluated *after* couchdb is installed and setup up
RABBITMQ_USER=$$(curl -sX GET $(CONFIG_DOC) | bin/json --raw get rabbitmq user)
RABBITMQ_HOST=$$(curl -sX GET $(CONFIG_DOC) | bin/json --raw get rabbitmq host)
RABBITMQ_PASSWORD=$$(curl -sX GET $(CONFIG_DOC) | bin/json --raw get rabbitmq password)



SCREEN_SESSION=feedshub
# FIXME: would be nice to have different bg colors for rabbit, couch etc.
SCREEN=screen -S $(SCREEN_SESSION)


SRC_COUCH=build/src/couchdb-0.9.0
OPT_COUCH=build/opt/couchdb-0.9.0


LISTEN_ORCHESTRATOR_PORT=7565
LISTEN_RABBIT_PORT=7566
LISTEN_COUCH_PORT=7567

ORCHESTRATOR_LOG=var/log/orchestrator.log
COUCH_LOG=var/log/couch.log
RABBIT_LOG=var/log/rabbit.log

COUCH_LISTENER_PIDFILE=var/run/couch-listener.pid
RABBIT_LISTENER_PIDFILE=var/run/rabbit-listener.pid
ORCHESTRATOR_LISTENER_PIDFILE=var/run/orchestrator-listener.pid
SHOWANDTELL_PIDSFILE=var/run/showandtell.pids

LISTEN_ORCHESTRATOR=while true; do sleep 1 && nc -l $(LISTEN_ORCHESTRATOR_PORT); done | tee -a $(ORCHESTRATOR_LOG)
LISTEN_RABBIT=while true; do sleep 1 && nc -k -l $(LISTEN_RABBIT_PORT); done | tee -a $(RABBIT_LOG)
LISTEN_COUCH=while true; do sleep 1 && nc -l $(LISTEN_COUCH_PORT); done | tee -a $(COUCH_LOG)

ORCHESTRATOR_FIFO=build/scratch/orchestrator_fifo
RABBIT_FIFO=build/scratch/rabbit_fifo
COUCH_FIFO=build/scratch/couch_fifo
PLUGIN_MAKEFILES=$(shell find plugins -maxdepth 2 -type f -name Makefile)

DEB_DEPENDENCIES=automake autoconf libtool help2man netcat-openbsd \
	build-essential erlang erlang-src libicu38 libicu-dev \
	libmozjs-dev libcurl4-openssl-dev mercurial subversion git\
	elinks python-simplejson cvs zip default-jdk \
	ant maven2 screen




default-target:
	@echo "Please choose a target from the makefile. (setup? update? all? clean? run?)"

setup: \
	install-debs \
	install-dev-debs \
	create-var-dirs \
	install-couchdb \
	install-erlang-rfc4627 \
	install-ibrowse \
	install-rabbitmq

###########################################################################

install-debs:
	: # none at the minute.

install-dev-debs:
	: # if everything is already installed, don't require sudo'ing
	- [ -z "`dpkg-query -W -f '$${status}' $(DEB_DEPENDENCIES) 2>&1 | grep -v 'install ok installed'`" ] || \
	( sudo apt-get update && sudo apt-get install $(DEB_DEPENDENCIES) )

###########################################################################

install-dist: install-erlang-rfc4627 install-ibrowse install-rabbitmq
	mkdir -p tmp/FeedsHub-0.1/erlang-rfc4627
	mkdir -p tmp/FeedsHub-0.1/ibrowse
	mkdir -p tmp/FeedsHub-0.1/rabbitmq
	mkdir -p tmp/FeedsHub-0.1/rabbitmq-erlang-client
	mkdir -p tmp/FeedsHub-0.1/orchestrator
	cp -rf build/opt/erlang-rfc4627/ebin tmp/FeedsHub-0.1/erlang-rfc4627
	cp -rf build/opt/ibrowse/ebin tmp/FeedsHub-0.1/ibrowse
	cp -rf build/opt/rabbitmq/ebin tmp/FeedsHub-0.1/rabbitmq
	cp -rf build/opt/rabbitmq-erlang-client/ebin tmp/FeedsHub-0.1/rabbitmq-erlang-client
	cp -rf orchestrator/ebin tmp/FeedsHub-0.1/orchestrator
	cp dist/orchestrator.sh tmp/FeedsHub-0.1
	cp dist/Makefile tmp/FeedsHub-0.1
	cd tmp; tar zcvf ../dist/FeedsHub-0.1.tar.gz FeedsHub-0.1

update: update-erlang-rfc4627 update-rabbitmq update-rabbitmq-erlang-client update-ibrowse

all:
	$(MAKE) -C orchestrator all
	$(MAKE) -C harness/java all
	for p in $(PLUGIN_MAKEFILES); \
		do $(MAKE) -C $$(dirname $$p) all; \
	done




docs:
	$(MAKE) -C doc

clean:
	$(MAKE) -C orchestrator clean
	$(MAKE) -C harness/java clean
	$(MAKE) -C doc clean
	for p in $(PLUGIN_MAKEFILES); \
		do $(MAKE) -C $$(dirname $$p) clean; \
	done
	rm -f couchdb.stderr couchdb.stdout

clean-dist:
	rm -rf tmp

cleandb:
	rm -rf $(OPT_COUCH)/var/lib/couchdb/*
	rm -rf $(OPT_COUCH)/var/log/couchdb/*
	rm -rf build/db/rabbitmq/*

veryclean: clean
	rm -rf var/log/* var/run/*
	rm -rf build/opt
	rm -rf build/scratch
	-[ -d $(SRC_COUCH) ] && $(MAKE) -C $(SRC_COUCH) clean
	-[ -d build/src/rabbitmq-server ] && $(MAKE) -C build/src/rabbitmq-server clean
	-[ -d build/src/rabbitmq-erlang-client ] && $(MAKE) -C build/src/rabbitmq-erlang-client clean

absurdlyclean: veryclean clean-dist cleandb
	rm -rf build
	rm -rf var

create-var-dirs:
	mkdir -p build/logs
	mkdir -p var/log
	mkdir -p var/run

###########################################################################
#


run: run-core run-orchestrator

run-core: run-couch run-rabbit

stop-core: stop-couch stop-rabbit

unlisten-core: unlisten-couch unlisten-rabbit


create-fresh-accounts:
	@echo 'Re-initializing RabbitMQ and CouchDB'
	@echo 'importing root_config into couchDB...'
	$(PYTHON) sbin/import_config.py --couchdb $(COUCH_SERVER) examples/basic_config
	@echo "(Re-)initializing RabbitMQ 'guest' and '$(shell echo $(RABBITMQ_USER))' accounts"
	-$(RABBITMQCTL) delete_user guest
	-$(RABBITMQCTL) delete_user $(RABBITMQ_USER)
	$(RABBITMQCTL) add_user $(RABBITMQ_USER) $(RABBITMQ_PASSWORD)
	$(RABBITMQCTL) set_permissions $(RABBITMQ_USER) '.*' '.*' '.*'

listen-orchestrator: create-var-dirs
	if ! ( test -e $(ORCHESTRATOR_LISTENER_PIDFILE)  &&  kill -0 "`cat $(ORCHESTRATOR_LISTENER_PIDFILE)`" )2>/dev/null; then \
		xterm -T orchestrator_listener -g 80x24-0+0700 -fg white -bg '#000040' -e "$(LISTEN_ORCHESTRATOR)" & \
		echo $$! > $(ORCHESTRATOR_LISTENER_PIDFILE); \
	fi

unlisten-orchestrator:
	- if [ -e $(ORCHESTRATOR_LISTENER_PIDFILE) ]; then \
		kill "`cat $(ORCHESTRATOR_LISTENER_PIDFILE)`"; rm -f $(ORCHESTRATOR_LISTENER_PIDFILE); \
	fi

listen-orchestrator-nox: create-var-dirs
	$(SCREEN) -X screen -t orchestrator_listener sh -c "$(LISTEN_ORCHESTRATOR)"



start-orchestrator: stop-orchestrator
	mkfifo $(ORCHESTRATOR_FIFO)
	( cat $(ORCHESTRATOR_FIFO) | \
	  ( $(MAKE) COUCH_SERVER=$(COUCH_SERVER) CONFIG_DOC=$(CONFIG_DOC) -C orchestrator run ; \
	    echo "Orchestrator died" ; \
	    pkill -x -f "nc localhost $(LISTEN_ORCHESTRATOR_PORT)" ) 2>&1 | \
	  nc localhost $(LISTEN_ORCHESTRATOR_PORT) > $(ORCHESTRATOR_FIFO) 2>&1 ; rm -f $(ORCHESTRATOR_FIFO) ) 2>/dev/null &
	sleep 6

run-orchestrator: listen-orchestrator start-orchestrator

listen-couch: create-var-dirs
	if ! ( test -e $(COUCH_LISTENER_PIDFILE)  &&  kill -0 "`cat $(COUCH_LISTENER_PIDFILE)`" )2>/dev/null; then \
		xterm -T couch_listener -g 80x24-0+0000 -fg white -bg '#400000' -e "$(LISTEN_COUCH)" &\
		echo $$! > $(COUCH_LISTENER_PIDFILE); \
	fi
unlisten-couch:
	- if [ -e $(COUCH_LISTENER_PIDFILE) ]; then \
		kill "`cat $(COUCH_LISTENER_PIDFILE)`"; rm -f $(COUCH_LISTENER_PIDFILE); \
	fi
listen-couch-nox: create-var-dirs
	$(SCREEN) -X screen -t couch_listener sh -c "$(LISTEN_COUCH)"


start-couch: stop-couch
	mkfifo $(COUCH_FIFO)
	( cat $(COUCH_FIFO) | \
	  ( $(OPT_COUCH)/bin/couchdb -i ; \
	    echo "CouchDb died" ; \
	    pkill -x -f "nc localhost $(LISTEN_COUCH_PORT)" ) 2>&1 | \
	  nc localhost $(LISTEN_COUCH_PORT) > $(COUCH_FIFO) 2>&1 ; rm -f $(COUCH_FIFO) ) 2>/dev/null &
	sleep 3

run-couch: listen-couch start-couch

listen-rabbit: create-var-dirs
	if ! ( test -e $(RABBIT_LISTENER_PIDFILE)  &&  kill -0 "`cat $(RABBIT_LISTENER_PIDFILE)`" )2>/dev/null; then \
		xterm -T rabbit_listener -g 80x24-0+0350 -fg white -bg '#004000' -e "$(LISTEN_RABBIT)" & \
		echo $$! > $(RABBIT_LISTENER_PIDFILE); \
	fi

unlisten-rabbit:
	- if [ -e $(RABBIT_LISTENER_PIDFILE) ]; then \
		kill "`cat $(RABBIT_LISTENER_PIDFILE)`"; rm -f $(RABBIT_LISTENER_PIDFILE); \
	fi

listen-rabbit-nox: create-var-dirs
	$(SCREEN) -X screen -t rabbit_listener sh -c "$(LISTEN_RABBIT)"


start-rabbit: stop-rabbit
	mkfifo $(RABBIT_FIFO)
	( cat $(RABBIT_FIFO) | \
	  ( ./start-feedshub-rabbit.sh ; \
	    echo "Myxomatosis" ; \
	    pkill -x -f "nc localhost $(LISTEN_RABBIT_PORT)" ) 2>&1 | \
	  nc localhost $(LISTEN_RABBIT_PORT) > $(RABBIT_FIFO) 2>&1 ; rm -f $(RABBIT_FIFO) ) 2>/dev/null &
	sleep 3

run-rabbit: listen-rabbit start-rabbit

listen-core: listen-couch listen-rabbit

listen-all: listen-orchestrator listen-core

unlisten-all: unlisten-orchestrator unlisten-couch unlisten-rabbit

unlisten-all-nox:
	FIXME

dummy-screen:
	@echo "starting up a screen session with 1-window per listener"
	$(SCREEN) -md -t dummy

listen-all-nox: dummy-screen listen-orchestrator-nox listen-couch-nox listen-rabbit-nox
	@echo '=================================================================='
	@echo 'Type ``screen -r`` to connect to a screen session w/ listeners for'
	@echo '1.  orchestrator'
	@echo '2.  couchDB'
	@echo '3.  rabbitMQ'

start-core: start-couch start-rabbit
	sleep 3

start-all: start-core start-orchestrator
	sleep 3


log-wipe-orchestrator:
	echo -n '' > $(ORCHESTRATOR_LOG)
log-wipe-couch:
	echo -n '' > $(COUCH_LOG)
log-wipe-rabbit:
	echo -n '' > $(RABBIT_LOG)
log-wipe-all: log-wipe-orchestrator log-wipe-couch log-wipe-rabbit


sleeper:
	sleep 2

stop-orchestrator:
	echo -e "\nok.\nq()." >> $(ORCHESTRATOR_FIFO)& sleep 3; (kill $$! 2>/dev/null || true)
	- pkill -x -f "nc localhost $(LISTEN_ORCHESTRATOR_PORT)"
	rm -f $(ORCHESTRATOR_FIFO)

stop-couch:
	echo -e "\nok.\nq()." >> $(COUCH_FIFO)& sleep 3; (kill $$! 2>/dev/null || true)
	- pkill -x -f "nc localhost $(LISTEN_COUCH_PORT)"
	rm -f $(COUCH_FIFO)

stop-rabbit:
	echo -e "\nok.\nq()." >> $(RABBIT_FIFO)& sleep 3; (kill $$! 2>/dev/null || true)
	- pkill -x -f "nc localhost $(LISTEN_RABBIT_PORT)"
	rm -f $(RABBIT_FIFO)

stop-all: stop-orchestrator stop-core

full-reset-core: stop-core cleandb start-core create-fresh-accounts

###########################################################################
# CouchDB

install-couchdb: $(SRC_COUCH) $(OPT_COUCH)

$(SRC_COUCH):
	@echo Checking out CouchDB from svn ...
	mkdir -p build/src && svn co http://svn.apache.org/repos/asf/couchdb/tags/0.9.0/ $(SRC_COUCH) \
		> build/logs/checkout-couchdb.txt 2>&1

$(OPT_COUCH):
	@echo Building CouchDB ...
	(cd $(SRC_COUCH); [ -f ./configure ] || ./bootstrap) \
		> build/logs/build-couchdb.txt 2>&1
	(cd $(SRC_COUCH); ./configure --prefix="$(CURDIR)/$(OPT_COUCH)" && $(MAKE) && $(MAKE) install) \
		>> build/logs/build-couchdb.txt 2>&1

###########################################################################
# Erlang RFC 4627

install-erlang-rfc4627: build/src/erlang-rfc4627 build/opt/erlang-rfc4627

update-erlang-rfc4627: build/src/erlang-rfc4627
	rm -rf build/opt/erlang-rfc4627
	(cd build/src/erlang-rfc4627 && hg pull && hg update)
	$(MAKE) build/opt/erlang-rfc4627

build/src/erlang-rfc4627:
	@echo Cloning erlang-rfc4627 ...
	(mkdir -p build/src && cd build/src && hg clone http://hg.opensource.lshift.net/erlang-rfc4627) \
		> build/logs/clone-erlang-rfc4627.txt 2>&1

build/opt/erlang-rfc4627:
	@echo Building erlang-rfc4627 ...
	(cd build/src/erlang-rfc4627 && $(MAKE)) \
		> build/logs/build-erlang-rfc4627.txt 2>&1
	(mkdir -p build/opt/erlang-rfc4627 && \
		cp -r build/src/erlang-rfc4627/{ebin,include} build/opt/erlang-rfc4627) \
		>> build/logs/build-erlang-rfc4627.txt 2>&1

###########################################################################
# iBrowse

install-ibrowse: build/src/ibrowse build/opt/ibrowse

update-ibrowse: build/src/ibrowse
	rm -rf build/opt/ibrowse
	(cd build/src/ibrowse && git fetch; git rebase origin)
	$(MAKE) build/opt/ibrowse

build/src/ibrowse:
	@echo checking out ibrowse
	(mkdir -p build/src && cd build/src && \
	 git clone git://github.com/cmullaparthi/ibrowse.git) > build/logs/clone-ibrowse.txt 2>&1

build/opt/ibrowse:
	(cd build/src/ibrowse/ && $(MAKE)) > build/logs/build-ibrowse.txt 2>&1
	(mkdir -p build/opt/ibrowse && \
		cp -r build/src/ibrowse/ebin build/opt/ibrowse/ebin) \
		>> build/logs/build-ibrowse.txt 2>&1

###########################################################################
# RabbitMQ

install-rabbitmq: \
	build/src/rabbitmq-codegen \
	build/src/rabbitmq-server \
	build/src/rabbitmq-erlang-client \
	build/opt/rabbitmq \
	build/opt/rabbitmq-erlang-client

update-rabbitmq: build/src/rabbitmq-codegen build/src/rabbitmq-server
	rm -rf build/scratch build/opt/rabbitmq
	(cd build/src/rabbitmq-codegen && hg pull && hg update -C default)
	(cd build/src/rabbitmq-server && hg pull && hg update -C default)
	$(MAKE) build/opt/rabbitmq

update-rabbitmq-erlang-client: build/src/rabbitmq-erlang-client
	rm -rf build/opt/rabbitmq-erlang-client
	(cd build/src/rabbitmq-erlang-client && hg pull && hg update)
	ln -sf `pwd`/build/src/rabbitmq-server build/src/rabbitmq_server
	(ERL_LIBS=`pwd`/build/src; export ERL_LIBS; $(MAKE) build/opt/rabbitmq-erlang-client)

build/src/rabbitmq-codegen:
	@echo Cloning rabbitmq-codegen ...
	(mkdir -p build/src && cd build/src && hg clone http://hg.rabbitmq.com/rabbitmq-codegen) \
		> build/logs/clone-rabbitmq-codegen.txt 2>&1

build/src/rabbitmq-server:
	@echo Cloning rabbitmq-server ...
	(mkdir -p build/src && cd build/src && hg clone http://hg.rabbitmq.com/rabbitmq-server) \
		> build/logs/clone-rabbitmq-server.txt 2>&1

build/src/rabbitmq-erlang-client:
	@echo Cloning rabbitmq-erlang-client ...
	(mkdir -p build/src && cd build/src && hg clone http://hg.rabbitmq.com/rabbitmq-erlang-client) \
		> build/logs/clone-rabbitmq-erlang-client.txt 2>&1

build/opt/rabbitmq:
	@echo Building rabbitmq-server ...
	(cd build/src/rabbitmq-server && $(MAKE) srcdist) \
		> build/logs/build-rabbitmq-server.txt 2>&1
	mkdir -p build/scratch \
		>> build/logs/build-rabbitmq-server.txt 2>&1
	(cd build/scratch && tar -zxvf ../src/rabbitmq-server/dist/rabbitmq-server-0.0.0.tar.gz) \
		>> build/logs/build-rabbitmq-server.txt 2>&1
	(cd build/scratch/rabbitmq-server-0.0.0 && $(MAKE) install TARGET_DIR="$(CURDIR)/build/opt/rabbitmq" SBIN_DIR="$(CURDIR)/build/opt/rabbitmq/sbin" MAN_DIR="$(CURDIR)/build/opt/rabbitmq/man") \
		>> build/logs/build-rabbitmq-server.txt 2>&1

build/opt/rabbitmq-erlang-client:
	@echo Building rabbitmq-erlang-client ...
	(cd build/src/rabbitmq-erlang-client && $(MAKE) BROKER_DIR=$(CURDIR)/build/opt/rabbitmq) \
		> build/logs/build-rabbitmq-erlang-client.txt 2>&1
	(mkdir -p build/opt/rabbitmq-erlang-client && \
		cp -r build/src/rabbitmq-erlang-client/{ebin,include} build/opt/rabbitmq-erlang-client) \
		>> build/logs/build-rabbitmq-erlang-client.txt 2>&1

###########################################################################
# Demos

demo-test: listen-all full-reset-core start-orchestrator
	sleep 5
	$(PYTHON) sbin/import_config.py --couchdb $(COUCH_SERVER) examples/test
	$(MAKE) start-orchestrator

demo-showandtell: full-reset-core demo-showandtell-stop start-orchestrator
	@echo 'Running show and tell demo'
	$(PYTHON) sbin/import_config.py --couchdb $(COUCH_SERVER) examples/showandtell_demo
	xterm -T 'Show&tell Listener' -g 80x24 -fg white -bg '#44dd00' -e 'nc -l 12345'& \
		echo $$! > $(SHOWANDTELL_PIDSFILE)
	sleep 1
	$(MAKE) start-orchestrator
	xterm -T 'Show&tell Producer' -g 80x24 -fg white -bg '#dd4400' -e 'while true; do nc localhost 45678 && sleep 1; done' & \
		echo $$! >> $(SHOWANDTELL_PIDSFILE)

demo-showandtell-stop: stop-orchestrator
	-kill `cat $(SHOWANDTELL_PIDSFILE)`
	-rm -f $(SHOWANDTELL_PIDSFILE)


test-plugins:
	@for d in plugins/*/tests; \
	   do echo "Testing plugin `dirname $${d}`"; \
	   for f in $${d}/*.io; \
	     do python bin/plugin_test_harness.py `dirname $${d}` -v --test $${f}; \
	   done \
	done