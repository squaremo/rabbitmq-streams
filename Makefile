SHELL=/bin/bash
PYTHON=python -Wignore::DeprecationWarning
RABBITMQCTL=./build/opt/rabbitmq/sbin/rabbitmqctl -q
RABBITSTREAMS_CONF=etc/rabbitstreams.conf
#HACK to work around problem in couchdb's configure on fedora 64bit
WITH_ERLANG=$(shell if test -d /usr/lib64/erlang/usr/include; \
	then echo "--with-erlang=/usr/lib64/erlang/usr/include"; fi)

CONFIG_DOC=$$(cat $(RABBITSTREAMS_CONF) | bin/json --raw get config_doc)
COUCH_SERVER=$$(echo $(CONFIG_DOC) | egrep -o 'http://[^/]+')
CONFIG_DB=$$(echo $(CONFIG_DOC) | sed -e 's/http:\/\/.*\/\(.*\)\/.*/\1/')

# !!!: this needs to evaluated *after* couchdb is installed and setup up
RABBITMQ_USER=$$(curl -sX GET $(CONFIG_DOC) | bin/json --raw get rabbitmq user)
RABBITMQ_HOST=$$(curl -sX GET $(CONFIG_DOC) | bin/json --raw get rabbitmq host)
RABBITMQ_PASSWORD=$$(curl -sX GET $(CONFIG_DOC) | bin/json --raw get rabbitmq password)
OS=$(shell if [ -e /etc/redhat-release ]; then echo "redhat"; \
	   elif [ -e /etc/debian_version ]; then echo "debian"; fi)

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
SCREEN_PIDFILE=var/run/screen.pid

LISTEN_ORCHESTRATOR=while true; do sleep 1 && nc -l $(LISTEN_ORCHESTRATOR_PORT); done | tee -a $(ORCHESTRATOR_LOG)
LISTEN_RABBIT=while true; do sleep 1 && nc -k -l $(LISTEN_RABBIT_PORT); done | tee -a $(RABBIT_LOG)
LISTEN_COUCH=while true; do sleep 1 && nc -l $(LISTEN_COUCH_PORT); done | tee -a $(COUCH_LOG)

ORCHESTRATOR_FIFO=build/scratch/orchestrator_fifo
RABBIT_FIFO=build/scratch/rabbit_fifo
COUCH_FIFO=build/scratch/couch_fifo
PLUGIN_MAKEFILES=$(shell find plugins -maxdepth 2 -type f -name Makefile)


DEB_AND_RPM_DEPENDENCIES:=curl automake autoconf libtool help2man erlang mercurial subversion git \
	ant maven2 screen python-simplejson cvs zip elinks

DEB_DEPENDENCIES:=${DEB_AND_RPM_DEPENDENCIES} \
        netcat-openbsd 	build-essential erlang-src libicu38 libicu-dev \
	libmozjs-dev libcurl4-openssl-dev default-jdk

RPM_DEPENDENCIES=${DEB_AND_RPM_DEPENDENCIES} nc gcc curl-devel icu libicu-devel \
	js-devel java-1.6.0-openjdk-devel \
	rpmdevtools

# Pin down 3rd party libs we get out of repos to a specific revision
RABBITMQ_HG_TAG=rabbitmq_v1_6_0
ERLANG_RFC4627_HG_TAG=a1d45d4ffdfb
RABBITMQ_CLIENT_HG_TAG=f3da1009b3cf
IBROWSE_GIT_TAG=9b0a927e39e7c3145a6cac11409144d3089f17f9
MOCHIWEB_SVN_TAG=104

# only needed for older fedora
MAVEN_SRC=http://apache.mirror.infiniteconflict.com/maven/binaries/apache-maven-2.0.10-bin.tar.bz2
GIVE_IT_TO_MY_FEDORA=

default-target:
	@echo "Please choose a target from the makefile. (setup? update? all? clean? run?)"

setup:
	$(MAKE) install-packages install-local-stuff

install-local-stuff: \
	create-var-dirs \
	install-couchdb \
	install-mochiweb \
	install-erlang-rfc4627 \
	install-ibrowse \
	install-rabbitmq

ifeq ($(OS),redhat)
export  JAVA_HOME=/usr/lib/jvm/java
endif

install-packages:
ifeq ($(OS),redhat)
	$(MAKE) install-rpms
else
	$(MAKE) install-debs
endif

install-rpms:
	sudo yum install -y $(RPM_DEPENDENCIES)
	${MAKE} give-it-to-my-fedora

give-it-to-my-fedora:
	@echo "WARNING: THIS HORRIBLE KLUDGE TO FIX VERSIONING PROBLEMS WITH FEDORA"; \
	 echo "         PACKAGES WILL JUST DUMP/OVERWRITE STUFF INTO /usr/local"; \
	 test -n "$(GIVE_IT_TO_MY_FEDORA)" || \
           (read -p "DO YOU GIVE YOU INFORMED CONSENT? (yes/No)? " ans; \
	    [ "$${ans}" = "yes" ] || (echo "ABORTING. Spoilsport!"; exit 1))
	if ! which escript >&/dev/null; then \
		sudo ln -s /usr/lib/erlang/bin/escript /usr/local/bin/; \
	fi; \
	if ! which erl_call >&/dev/null; then \
		sudo ln -s /usr/lib/erlang/lib/erl_interface-*/bin/erl_call /usr/local/bin; \
	fi; \
	if  ! mvn --version | grep 'Maven version: ' | \
	   	python -c 'import sys; \
sys.exit(not map(int,sys.stdin.read().split(": ")[1].split("."))>[2,0,6])'; then \
		wget $(MAVEN_SRC) -O- | (cd /usr/local/; tar xj --strip-components 1);\
		yum -y remove maven2; \
	fi
###########################################################################
install-debs:
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

all: all-orchestrator all-harnesses all-plugins

# run this to "make" after code changes
test: all
	$(MAKE) test-plugins

all-orchestrator:
	$(MAKE) -C orchestrator all

all-harnesses:
	$(MAKE) -C harness/java all

all-plugins: all-harnesses
	$(MAKE) -C plugins all

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
	rm -f  orchestrator/deps/*
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


run:
	$(MAKE) run-core run-orchestrator

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
	  ( $(MAKE) COUCH_SERVER=$(COUCH_SERVER) CONFIG_DOC=$(CONFIG_DOC) CONFIG_DB=$(CONFIG_DB) -C orchestrator run ; \
	    echo "Orchestrator died" ; \
	    pkill -x -f "nc localhost $(LISTEN_ORCHESTRATOR_PORT)" ) 2>&1 | \
	  nc localhost $(LISTEN_ORCHESTRATOR_PORT) > $(ORCHESTRATOR_FIFO) 2>&1 ; rm -f $(ORCHESTRATOR_FIFO) ) 2>/dev/null &
	sleep 6

run-orchestrator:
	$(MAKE) listen-orchestrator start-orchestrator

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

run-couch: listen-couch
	$(MAKE) start-couch

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

run-rabbit: listen-rabbit
	$(MAKE) start-rabbit

listen-core: listen-couch listen-rabbit

listen-all: listen-orchestrator listen-core

unlisten-all: unlisten-orchestrator unlisten-couch unlisten-rabbit

unlisten-all-nox:
	kill "`cat $(SCREEN_PIDFILE)`" && rm -f "$(SCREEN_PIDFILE)"

dummy-screen:
	@echo "starting up a screen session with 1-window per listener"
	$(SCREEN) -md -t dummy
	# FIXME pretty horrible hack, race condition etc.
	$(SCREEN) -ls | grep Detached | head -1 | egrep -o '^	[0-9]+' | cut -f2 > $(SCREEN_PIDFILE)

listen-all-nox: dummy-screen listen-orchestrator-nox listen-couch-nox listen-rabbit-nox
	@echo '=================================================================='
	@echo 'Type ``screen -r`` to connect to a screen session w/ listeners for'
	@echo '1.  orchestrator'
	@echo '2.  couchDB'
	@echo '3.  rabbitMQ'

start-core: start-couch start-rabbit
	sleep 3

start-all: start-core
	$(MAKE) start-orchestrator
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
	(cd $(SRC_COUCH); ./configure "$(WITH_ERLANG)" --prefix="$(CURDIR)/$(OPT_COUCH)" && $(MAKE) && $(MAKE) install) \
		>> build/logs/build-couchdb.txt 2>&1


###########################################################################
# Erlang RFC 4627

install-erlang-rfc4627: orchestrator/deps/erlang-rfc4627

update-erlang-rfc4627: build/src/erlang-rfc4627
	rm -rf build/opt/erlang-rfc4627
	(cd build/src/erlang-rfc4627 && hg pull -r $(ERLANG_RFC4627_HG_TAG) && hg update)
	$(MAKE) build/opt/erlang-rfc4627

build/src/erlang-rfc4627:
	@echo Cloning erlang-rfc4627 ...
	(mkdir -p build/src && cd build/src && hg clone http://hg.opensource.lshift.net/erlang-rfc4627) \
		> build/logs/clone-erlang-rfc4627.txt 2>&1

build/opt/erlang-rfc4627: build/src/erlang-rfc4627
	@echo Building erlang-rfc4627 ...
	(cd $< && $(MAKE)) >& build/logs/build-erlang-rfc4627.txt
	(mkdir -p $@ && cp -r $</{ebin,include} $@) >> build/logs/build-erlang-rfc4627.txt 2>&1

orchestrator/deps/erlang-rfc4627: build/opt/erlang-rfc4627
	ln -s ../../$< $@

###########################################################################
# iBrowse

install-ibrowse: orchestrator/deps/ibrowse

update-ibrowse: build/src/ibrowse
	rm -rf build/opt/ibrowse
	(cd build/src/ibrowse && git fetch; git checkout -f $(IBROWSE_GIT_TAG))
	$(MAKE) build/opt/ibrowse

build/src/ibrowse:
	@echo checking out ibrowse
	(mkdir -p build/src && cd build/src && \
	 git clone -n git://github.com/cmullaparthi/ibrowse.git && \
         cd ibrowse && git checkout -f $(IBROWSE_GIT_TAG)) > build/logs/clone-ibrowse.txt 2>&1

build/opt/ibrowse: build/src/ibrowse
	(cd $< && $(MAKE)) > build/logs/build-ibrowse.txt 2>&1
	(mkdir -p $@ && cp -r $</ebin $@/ebin) >> build/logs/build-ibrowse.txt 2>&1

orchestrator/deps/ibrowse: build/opt/ibrowse
	ln -s ../../$< $@

###########################################################################
# Mochiweb

install-mochiweb: orchestrator/deps/mochiweb

build/src/mochiweb:
	@echo Checking out Mochiweb from svn ...
	(mkdir -p build/src && cd build/src && \
            svn co -r $(MOCHIWEB_SVN_TAG) http://mochiweb.googlecode.com/svn/trunk/ mochiweb) \
		> build/logs/checkout-mochiweb.txt 2>&1

build/opt/mochiweb: build/src/mochiweb
	@echo Building Mochiweb ...
	(cd $<; $(MAKE) all) > build/logs/build-mochiweb.txt 2>&1
	(mkdir -p $@ && cp -r $</{ebin,include} $@)

orchestrator/deps/mochiweb: build/opt/mochiweb
	ln -s ../../$< $@



###########################################################################
# RabbitMQ

install-rabbitmq: orchestrator/deps/rabbitmq orchestrator/deps/rabbitmq-erlang-client

update-rabbitmq: build/src/rabbitmq-codegen build/src/rabbitmq-server
	rm -rf build/scratch build/opt/rabbitmq
	(cd build/src/rabbitmq-codegen && hg pull -r $(RABBITMQ_HG_TAG) && hg update -C default)
	(cd build/src/rabbitmq-server && hg pull -r $(RABBITMQ_HG_TAG) && hg update -C default)
	$(MAKE) build/opt/rabbitmq

update-rabbitmq-erlang-client: build/src/rabbitmq-erlang-client
	rm -rf build/opt/rabbitmq-erlang-client
	(cd build/src/rabbitmq-erlang-client && hg pull -r $(RABBITMQ_CLIENT_HG_TAG) && hg update)
	ln -sf `pwd`/build/src/rabbitmq-server build/src/rabbitmq_server
	(ERL_LIBS=`pwd`/build/src; export ERL_LIBS; $(MAKE) build/opt/rabbitmq-erlang-client)

build/src/rabbitmq-codegen:
	@echo Cloning rabbitmq-codegen ...
	(mkdir -p build/src && cd build/src && \
         hg clone -r $(RABBITMQ_HG_TAG) http://hg.rabbitmq.com/rabbitmq-codegen ) \
		> build/logs/clone-rabbitmq-codegen.txt 2>&1

build/src/rabbitmq-server:
	@echo Cloning rabbitmq-server ...
	(mkdir -p build/src && cd build/src && \
	 hg clone -r $(RABBITMQ_HG_TAG) http://hg.rabbitmq.com/rabbitmq-server) \
		> build/logs/clone-rabbitmq-server.txt 2>&1

build/src/rabbitmq-erlang-client:
	@echo Cloning rabbitmq-erlang-client ...
	(mkdir -p build/src && cd build/src && \
         hg clone -r $(RABBITMQ_CLIENT_HG_TAG) http://hg.rabbitmq.com/rabbitmq-erlang-client) \
		> build/logs/clone-rabbitmq-erlang-client.txt 2>&1

build/opt/rabbitmq: build/src/rabbitmq-server build/src/rabbitmq-codegen
	@echo Building rabbitmq-server ...
	(cd $< && $(MAKE) srcdist) > build/logs/build-rabbitmq-server.txt 2>&1
	mkdir -p build/scratch >> build/logs/build-rabbitmq-server.txt 2>&1
	(cd build/scratch && tar -zxvf ../../$</dist/rabbitmq-server-0.0.0.tar.gz) \
		>> build/logs/build-rabbitmq-server.txt 2>&1
	(cd build/scratch/rabbitmq-server-0.0.0 && \
	 $(MAKE) install TARGET_DIR="$(CURDIR)/$@" SBIN_DIR="$(CURDIR)/$@/sbin" MAN_DIR="$(CURDIR)/$@/man") \
		>> build/logs/build-rabbitmq-server.txt 2>&1

build/opt/rabbitmq-erlang-client: build/src/rabbitmq-erlang-client build/opt/rabbitmq
	@echo Building rabbitmq-erlang-client ...
	(cd $< && $(MAKE) BROKER_DIR=$(CURDIR)/$(word 2,$^)) \
		> build/logs/build-rabbitmq-erlang-client.txt 2>&1
	(mkdir -p $@ && cp -r $</{ebin,include} $@) \
		>> build/logs/build-rabbitmq-erlang-client.txt 2>&1

orchestrator/deps/rabbitmq-erlang-client: build/opt/rabbitmq-erlang-client
	ln -s ../../$< $@

orchestrator/deps/rabbitmq: build/opt/rabbitmq
	ln -s ../../$< $@


###########################################################################
# Demos

demo-test:
	$(MAKE) listen-all full-reset-core start-orchestrator
	sleep 5
	$(PYTHON) sbin/import_config.py --couchdb $(COUCH_SERVER) examples/test
	$(MAKE) start-orchestrator

demo-showandtell:
	$(MAKE) full-reset-core  demo-showandtell-stop start-orchestrator
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



test-plugins: start-all
	bin/test_plugins plugins/*/
