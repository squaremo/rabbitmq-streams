include support/include.mk

all: doc_dir $(EBIN_FILES)

doc_dir:
	mkdir -p $(DOC_DIR)

clean:
	rm -rf $(EBIN_FILES)
	rm -f erl_crash.dump

cleandb:
	http_proxy= curl -X DELETE http://localhost:5984/feedshub_config

# TODO(alexander): -the orchestrator should probably pull COUCH_SERVER itself;
#                  -the variables COUCH_SERVER and CONFIG_DOC need to be passed in
run: all
	$(ERL)  -pa ebin/ -boot start_sasl \
		-sname orchestrator -s crypto -s orchestrator \
		-orchestrator couch_base_url '"$(COUCH_SERVER)/"'  \
		-orchestrator root_config_url '"$(CONFIG_DOC)"' \
		-orchestrator config_db '"$(CONFIG_DB)"'
