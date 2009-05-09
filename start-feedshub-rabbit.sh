#!/bin/bash

export RABBITMQ_LOG_BASE="`pwd`/build/logs/rabbitmq"
export RABBITMQ_MNESIA_BASE="`pwd`/build/db/rabbitmq"
export RABBITMQ_NODE_ONLY=true
export RABBITMQ_SERVER_START_ARGS="${RABBITMQ_SERVER_START_ARGS} -s rabbit"

mkdir -p "$RABBITMQ_LOG_BASE"

cd build/opt/rabbitmq
./sbin/rabbitmq-server
