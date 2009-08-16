## -*- makefile -*-

######################################################################
## Erlang

ERL := erl
ERLC := $(ERL)c

RFC4627_BIN=$(CURDIR)/../../build/opt/erlang-rfc4627
ERLAMQP_BIN=$(CURDIR)/../../build/opt/rabbitmq-erlang-client
RABBIT_BIN=$(CURDIR)/../../build/opt/rabbitmq
IBROWSE_BIN=$(CURDIR)/../../build/opt/ibrowse

INCLUDE_EXT := -I $(RFC4627_BIN)/include -I $(ERLAMQP_BIN)/include -I $(RABBIT_BIN)/include
ERL_PATH_EXT := -pa $(RFC4627_BIN)/ebin -pa $(ERLAMQP_BIN)/ebin -pa $(RABBIT_BIN)/ebin -pa $(IBROWSE_BIN)/ebin

INCLUDE_DIRS := $(INCLUDE_EXT) ../include $(wildcard ../deps/*/include)
EBIN_DIRS := $(wildcard ../deps/*/ebin)
ERLC_FLAGS := -W $(INCLUDE_DIRS:../%=-I ../%) $(EBIN_DIRS:%=-pa %) $(ERL_PATH_EXT)

ifndef no_debug_info
  ERLC_FLAGS += +debug_info
endif

ifdef debug
  ERLC_FLAGS += -Ddebug
endif

EBIN_DIR := ../ebin
DOC_DIR  := ../doc
EMULATOR := beam

ERL_SOURCES := $(wildcard *.erl)
ERL_HEADERS := $(wildcard *.hrl) $(wildcard ../include/*.hrl)
ERL_OBJECTS := $(ERL_SOURCES:%.erl=$(EBIN_DIR)/%.$(EMULATOR))
ERL_DOCUMENTS := $(ERL_SOURCES:%.erl=$(DOC_DIR)/%.html)
ERL_OBJECTS_LOCAL := $(ERL_SOURCES:%.erl=./%.$(EMULATOR))
APP_FILES := $(wildcard *.app)
EBIN_FILES = $(ERL_OBJECTS) $(ERL_DOCUMENTS) $(APP_FILES:%.app=../ebin/%.app)
EBIN_FILES_NO_DOCS = $(ERL_OBJECTS) $(APP_FILES:%.app=../ebin/%.app)
MODULES = $(ERL_SOURCES:%.erl=%)

../ebin/%.app: %.app
	cp $< $@

$(EBIN_DIR)/%.$(EMULATOR): %.erl
	$(ERLC) $(ERLC_FLAGS) -o $(EBIN_DIR) $<

./%.$(EMULATOR): %.erl
	$(ERLC) $(ERLC_FLAGS) -o . $<

$(DOC_DIR)/%.html: %.erl
	$(ERL) -noshell -run edoc file $< -run init stop
	mv *.html $(DOC_DIR)
