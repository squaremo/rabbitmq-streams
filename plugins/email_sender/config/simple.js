var socketin = create_server("socket_in", "socket_source", {"host": "localhost"});

var emailout = create_server("email_out", "email_sender", {
			       "host": "smtp.lshift.net",
			       "transportProtocol": "smtp",
			       "username": "",
			       "password": ""});

var socketout = create_server("socket_out", "socket_destination", {});

var input = declare_terminal("in");
consume(socketin, {"port": 45670}, input);

var output = declare_terminal("out");
subscribe(emailout, {"to": ["junk@lshift.net"], "subject": "simple test"}, output);
subscribe(socketout, {"port": 12340, "host": "0.0.0.0"}, output);

var middle = feed("middle_bit");
middle.connect(middle.terminal(input), middle.terminal(output));
middle.connect(middle.terminal(input),
	       resource(middle.plugin({"type":"logger", "configuration": {}}),
				      "input"));

setAllActive();
