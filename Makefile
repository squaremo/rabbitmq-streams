SHELL=/bin/bash
RABBITMQCTL=./build/opt/rabbitmq/sbin/rabbitmqctl -q

SCREEN_SESSION=feedshub
# FIXME: would be nice to have different bg colors for rabbit, couch etc.
SCREEN=screen -S $(SCREEN_SESSION)


SRC_COUCH=build/src/couchdb-0.9.0
OPT_COUCH=build/opt/couchdb-0.9.0


LISTEN_ORCH_PORT := 7565
LISTEN_RABBIT_PORT := 7566
LISTEN_COUCH_PORT := 7567

LISTEN_ORCHESTRATOR=while true; do sleep 1 && nc -l $(LISTEN_ORCH_PORT); done | tee -a orch_log
LISTEN_RABBIT=while true; do sleep 1 && nc -k -l $(LISTEN_RABBIT_PORT); done
LISTEN_COUCH=while true; do sleep 1 && nc -l $(LISTEN_COUCH_PORT); done

ORCH_FIFO := build/scratch/orch_fifo
RABBIT_FIFO := build/scratch/rabbit_fifo
COUCH_FIFO := build/scratch/couch_fifo
PLUGIN_MAKEFILES := $(shell find plugins -maxdepth 2 -type f -name Makefile)

default_target:
	@echo "Please choose a target from the makefile. (setup? update? all? clean? run?)"

setup: \
	install-debs \
	install-dev-debs \
	create-build-logs-dir \
	install-couchdb \
	install-erlang-rfc4627 \
	install-ibrowse \
	install-rabbitmq

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

cleandb:
	rm -rf $(OPT_COUCH)/var/lib/couchdb/*
	rm -rf $(OPT_COUCH)/var/log/couchdb/*
	rm -rf build/db/rabbitmq/*

veryclean: clean
	rm -rf build/opt
	rm -rf build/scratch
	-[ -d $(SRC_COUCH) ] && $(MAKE) -C $(SRC_COUCH) clean
	-[ -d build/src/rabbitmq-server ] && $(MAKE) -C build/src/rabbitmq-server clean
	-[ -d build/src/rabbitmq-erlang-client ] && $(MAKE) -C build/src/rabbitmq-erlang-client clean

absurdlyclean:
	rm -rf build

run: run_core
	xterm -g 80x24-0+0700 -fg white -bg '#000040' -e "$(MAKE) -C orchestrator run" &

run_core:
	$(OPT_COUCH)/bin/couchdb -b
	(xterm -g 80x24-0+0000 -fg white -bg '#400000' -e "sleep 3; tail -f couchdb.std* $(OPT_COUCH)/var/log/couchdb/*" & (echo $$! > couchlogtail.pid))
	xterm -g 80x24-0+0350 -fg white -bg '#004000' -e "./start-feedshub-rabbit.sh" &
	sleep 3

stop_core:
	-$(OPT_COUCH)/bin/couchdb -d
	-kill `cat couchlogtail.pid`; rm couchlogtail.pid
	-build/opt/rabbitmq/sbin/rabbitmqctl stop
	sleep 3

full_reset_core:
	$(MAKE) stop_core cleandb run_core create_fresh_accounts

create-build-logs-dir:
	mkdir -p build/logs

create_fresh_accounts:
	@echo 'Re-initializing RabbitMQ with fresh `guest` and `feedshub_admin` accounts'
	-@$(RABBITMQCTL) delete_user guest
	-@$(RABBITMQCTL) delete_user feedshub_admin
	@$(RABBITMQCTL) add_user feedshub_admin feedshub_admin
	@$(RABBITMQCTL) set_permissions feedshub_admin '.*' '.*' '.*'


###########################################################################
# Run alternatives which don't create xterms unless you want them to

listen_orchestrator:
	xterm -g 80x24-0+0700 -fg white -bg '#000040' -e "$(LISTEN_ORCHESTRATOR)" &

listen_orchestrator_nox:
	$(SCREEN) -X screen -t orchestrator_listener sh -c "$(LISTEN_ORCHESTRATOR)"

start_orchestrator_nox: stop_orchestrator_nox
	mkfifo $(ORCH_FIFO)
	( cat $(ORCH_FIFO) | \
	  ( $(MAKE) -C orchestrator run ; \
	    echo "Orchestrator died" ; \
	    pkill -x -f "nc localhost $(LISTEN_ORCH_PORT)" ) 2>&1 | \
	  nc localhost $(LISTEN_ORCH_PORT) > $(ORCH_FIFO) 2>&1 ; rm -f $(ORCH_FIFO) ) 2>/dev/null &

listen_couch:
	xterm -g 80x24-0+0000 -fg white -bg '#400000' -e "$(LISTEN_COUCH)" &
listen_couch_nox:
	$(SCREEN) -X screen -t couch_listener sh -c "$(LISTEN_COUCH)"



start_couch_nox: stop_couch_nox
	mkfifo $(COUCH_FIFO)
	( cat $(COUCH_FIFO) | \
	  ( $(OPT_COUCH)/bin/couchdb -i ; \
	    echo "CouchDb died" ; \
	    pkill -x -f "nc localhost $(LISTEN_COUCH_PORT)" ) 2>&1 | \
	  nc localhost $(LISTEN_COUCH_PORT) > $(COUCH_FIFO) 2>&1 ; rm -f $(COUCH_FIFO) ) 2>/dev/null &

listen_rabbit:
	xterm -g 80x24-0+0350 -fg white -bg '#004000' -e "$(LISTEN_RABBIT)" &
listen_rabbit_nox:
	$(SCREEN) -X screen -t rabbit_listener sh -c "$(LISTEN_RABBIT)"


start_rabbit_nox: stop_rabbit_nox
	mkfifo $(RABBIT_FIFO)
	( cat $(RABBIT_FIFO) | \
	  ( ./start-feedshub-rabbit.sh ; \
	    echo "Myxomatosis" ; \
	    pkill -x -f "nc localhost $(LISTEN_RABBIT_PORT)" ) 2>&1 | \
	  nc localhost $(LISTEN_RABBIT_PORT) > $(RABBIT_FIFO) 2>&1 ; rm -f $(RABBIT_FIFO) ) 2>/dev/null &

listen_all: listen_orchestrator listen_couch listen_rabbit

dummy_screen:
	@echo "starting up a screen session with 1-window per listener"
	$(SCREEN) -md -t dummy

listen_all_nox: dummy_screen listen_orchestrator_nox listen_couch_nox listen_rabbit_nox
	@echo '=================================================================='
	@echo 'Type ``screen -r`` to connect to a screen session w/ listeners for'
	@echo '1.  orchestrator'
	@echo '2.  couchDB'
	@echo '3.  rabbitMQ'

start_core_nox: start_couch_nox start_rabbit_nox

start_all_nox: start_core_nox sleeper start_orchestrator_nox

sleeper:
	sleep 2

stop_orchestrator_nox:
	echo -e "\nok.\nq()." >> $(ORCH_FIFO) && sleep 3
	- pkill -x -f "nc localhost $(LISTEN_ORCH_PORT)"
	rm -f $(ORCH_FIFO)

stop_couch_nox:
	echo -e "\nok.\nq()." >> $(COUCH_FIFO) && sleep 3
	- pkill -x -f "nc localhost $(LISTEN_COUCH_PORT)"
	rm -f $(COUCH_FIFO)

stop_rabbit_nox:
	echo -e "\nok.\nq()." >> $(RABBIT_FIFO) && sleep 3
	- pkill -x -f "nc localhost $(LISTEN_RABBIT_PORT)"
	rm -f $(RABBIT_FIFO)

stop_core_nox:
	echo -e "\nok.\nq()." >> $(RABBIT_FIFO)
	echo -e "\nok.\nq()." >> $(COUCH_FIFO)
	sleep 3
	- pkill -x -f "nc localhost $(LISTEN_RABBIT_PORT)"
	- pkill -x -f "nc localhost $(LISTEN_COUCH_PORT)"
	rm -f $(RABBIT_FIFO)
	rm -f $(COUCH_FIFO)

stop_all_nox: stop_orchestrator_nox stop_core_nox

full_reset_core_nox:
	$(MAKE) stop_core_nox cleandb start_core_nox sleeper create_fresh_accounts

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

install-ibrowse: build/src/jungerl build/opt/ibrowse

update-ibrowse: build/src/jungerl
	rm -rf build/opt/ibrowse
	(cd build/src/jungerl && cvs update -Pd)
	$(MAKE) build/opt/ibrowse

build/src/jungerl:
	@echo checking out jungerl
	(mkdir -p build/src && cd build/src && \
	 cvs -d:pserver:anonymous@jungerl.cvs.sourceforge.net:/cvsroot/jungerl login && \
	 cvs -z3 -d:pserver:anonymous@jungerl.cvs.sourceforge.net:/cvsroot/jungerl co -P jungerl) \
		> build/logs/clone-jungerl.txt 2>&1

build/opt/ibrowse:
	(cd build/src/jungerl/lib/ibrowse/src && $(MAKE)) \
		> build/logs/build-ibrowse.txt 2>&1
	(mkdir -p build/opt/ibrowse && \
		cp -r build/src/jungerl/lib/ibrowse/ebin build/opt/ibrowse) \
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
	(cd build/src/rabbitmq-codegen && hg pull && hg update)
	(cd build/src/rabbitmq-server && hg pull && hg update)
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

install-debs:
	: # none at the minute.

install-dev-debs:
	: # these come from the couchdb README.
	- sudo apt-get update && sudo apt-get install \
                               automake autoconf libtool help2man netcat-openbsd \
	                       build-essential erlang libicu38 libicu-dev \
	                       libmozjs-dev libcurl4-openssl-dev mercurial subversion \
	                       elinks python-simplejson cvs zip default-jdk ant maven2 screen

###########################################################################

install-dist:
	mkdir -p tmp/FeedsHub-0.1/erlang-rfc4627
	mkdir -p tmp/FeedsHub-0.1/ibrowse
	mkdir -p tmp/FeedsHub-0.1/rabbitmq
	mkdir -p tmp/FeedsHub-0.1/rabbitmq-erlang-client
	cp -rf build/opt/erlang-rfc4627/ebin tmp/FeedsHub-0.1/erlang-rfc4627	
	cp -rf build/opt/ibrowse/ebin tmp/FeedsHub-0.1/ibrowse
	cp -rf build/opt/rabbitmq/ebin tmp/FeedsHub-0.1/rabbitmq
	cp -rf build/opt/rabbitmq-erlang-client/ebin tmp/FeedsHub-0.1/rabbitmq-erlang-client
	cp dist/Makefile tmp/FeedsHub-0.1
	cd tmp; tar zcvf ../dist/FeedsHub-0.1.tar.gz FeedsHub-0.1

clean-dist:
	rm -rf tmp
