#!/bin/bash

echo "Setting up CouchDB and RabbitMQ."

CTL="build/opt/rabbitmq/sbin/rabbitmqctl -q"

$CTL delete_user guest

$CTL add_user feedshub_admin feedshub_admin
$CTL set_permissions feedshub_admin '.*' '.*' '.*'
