SOURCE_DIR=src
EBIN_DIR=ebin
INCLUDE_DIR=include
SOURCES=$(wildcard $(SOURCE_DIR)/*.erl)
BEAM_TARGETS=$(patsubst $(SOURCE_DIR)/%.erl, $(EBIN_DIR)/%.beam,$(SOURCES))
TARGETS=$(BEAM_TARGETS)

RFC4627_BIN=$(CURDIR)/../build/opt/erlang-rfc4627
ERLAMQP_BIN=$(CURDIR)/../build/opt/rabbitmq-erlang-client
RABBIT_BIN=$(CURDIR)/../build/opt/rabbitmq

INCLUDE_OPTS=-I $(INCLUDE_DIR) -I $(RFC4627_BIN)/include -I $(ERLAMQP_BIN)/include -I $(RABBIT_BIN)/include

ERLC_OPTS=$(INCLUDE_OPTS) -o $(EBIN_DIR) -Wall -v +debug_info

ERLC=erlc
ERL=erl

ERL_PATH_OPTS=-pa $(EBIN_DIR) -pa $(RFC4627_BIN)/ebin -pa $(ERLAMQP_BIN)/ebin -pa $(RABBIT_BIN)/ebin

all: $(TARGETS)

$(EBIN_DIR)/%.beam: $(SOURCE_DIR)/%.erl
	$(ERLC) $(ERLC_OPTS) -pa $(EBIN_DIR) $<

clean:
	rm -f $(EBIN_DIR)/*.beam
	rm -f erl_crash.dump

run: all
	$(ERL) $(ERL_PATH_OPTS) \
		-boot start_sasl \
		-sname orchestrator -s orchestrator
