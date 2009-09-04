// Standard endpoints for using in test streams.

var http = create_server("http", "httppost", {"http_server_port": 9876});
var socketout = create_server("netprinter", "socket_destination", {});

var httppost_a = declare_terminal("httppost_a");
consume(http, {"url_path": "/foo"}, httppost_a);

var netprint12345 = declare_terminal("netprint12345");
subscribe(socketout, {"port": "12345", "host": "127.0.0.1"}, netprint12345);

setAllActive();
