num_receivers = 2;

base_port = 12345;

var socketout = create_server("netprinter", "socket_destination", {});

var http = create_server("http", "httppost", {"http_server_port": 9876});
var httppost_a = declare_terminal("httppost_a");
consume(http, {"url_path": "/foo"}, httppost_a);

var f = feed("multi");
var j = f.plugin({"type": "javascript",
  "configuration": {"function": "function (x) { return x; }"}});
var input = f.terminal(httppost_a);
f.connect(input, resource(j, "input"));
jout = resource(j, "output");

for (var i=0; i<num_receivers; i++) {
  var port = base_port + i;
  var listener = declare_terminal("netprinter_"+port);
  subscribe(socketout, {"port": port, "host": "0.0.0.0"}, listener);
  f.connect(jout, f.terminal(listener));
  //f.connect(input, listener);
}

setAllActive();
