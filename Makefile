all:
	@echo "Please choose a target from the Makefile."

install-debs:
	sudo apt-get install daemontools-run ejabberd rabbitmq-server