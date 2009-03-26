#!/bin/bash

export RABBITMQ_LOG_BASE="`pwd`/build/logs/rabbitmq"
export RABBITMQ_MNESIA_BASE="`pwd`/build/db/rabbitmq"

mkdir -p "$RABBITMQ_LOG_BASE"

cd build/opt/rabbitmq
./sbin/rabbitmq-server
