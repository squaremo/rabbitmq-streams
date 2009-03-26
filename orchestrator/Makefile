SOURCE_DIR=src
EBIN_DIR=ebin
INCLUDE_DIR=include
SOURCES=$(wildcard $(SOURCE_DIR)/*.erl)
BEAM_TARGETS=$(patsubst $(SOURCE_DIR)/%.erl, $(EBIN_DIR)/%.beam,$(SOURCES))
TARGETS=$(BEAM_TARGETS)

ERLC_OPTS=-I $(INCLUDE_DIR) -o $(EBIN_DIR) -Wall -v +debug_info

ERLC=erlc
ERL=erl

all: $(TARGETS)

$(EBIN_DIR)/%.beam: $(SOURCE_DIR)/%.erl
	$(ERLC) $(ERLC_OPTS) -pa $(EBIN_DIR) $<

clean:
	rm -f $(EBIN_DIR)/*.beam
	rm -f erl_crash.dump

run: all
	$(ERL) -pa $(EBIN_DIR) -sname orchestrator -s orchestrator
