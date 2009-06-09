erl -pa LIB_DIR/orchestrator/ebin \
    -pa LIB_DIR/erlang-rfc4627/ebin \
    -pa LIB_DIR/rabbitmq-erlang-client/ebin \
    -pa LIB_DIR/rabbitmq/ebin \
    -pa LIB_DIR/ibrowse/ebin \
    -boot start_sasl \
    -sname orchestrator -s orchestrator
