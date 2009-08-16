
all:
	(cd src; $(MAKE))

clean:
	(cd src; $(MAKE) clean)
	rm -f erl_crash.dump

cleandb:
	http_proxy= curl -X DELETE http://localhost:5984/feedshub_config

# TODO(alexander): -the orchestrator should probably pull COUCH_SERVER itself;
#                  -the variables COUCH_SERVER and CONFIG_DOC need to be passed in
run: all
	$(ERL) $(ERL_PATH_OPTS) \
		-boot start_sasl \
		-sname orchestrator -s orchestrator \
		-orchestrator couch_base_url '"$(COUCH_SERVER)/"'  \
		-orchestrator root_config_url '"$(CONFIG_DOC)"'
