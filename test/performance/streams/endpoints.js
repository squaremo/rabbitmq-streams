// Standard endpoints for using in test streams.

var http = create_server("http", "httppost", {"http_server_port": 8888});
var socketout = create_server("netprinter", "socket_destination", {});

var httppost_a = declare_terminal("httppost_a");
consume(http, {"url_path": "/a"}, httppost_a);

var netprint12345 = declare_terminal("netprint12345");
subscribe(socketout, {"port": "12345", "host": "0.0.0.0"}, netprint12345);
