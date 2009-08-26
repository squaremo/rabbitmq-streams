// This produces a chain of no-op JavaScript plugins arranged in a line.
//
// The purpose is to see how Streams performs when
// 1. running one JVM per plugin
// 2. running one JVM per feed

var links = 1;

var f = feed("chain");

var input = f.terminal(httppost_a);
var output = f.terminal(netprint12345);

//var log = f.plugin({"type": "logger", "configuration": {}});

//f.connect(input, resource(log, "input"));

function js() {
  return f.plugin({"type": "javascript",
                   "configuration":
                   {"function": "function(msg) {return msg;}"}});
}

function split() {
  return f.plugin({"type": "regexp_split",
                   "configuration":
                   {"regexp": ".*"}});
}

function js_conn(p, other) {
  f.connect(resource(p, "output"), other);
}

function split_conn(p, other) {
  f.connect(resource(p, "positive"), other);
  f.connect(resource(p, "negative"), other);
}

var plugin = split;
var connect = split_conn;

var p = plugin();
f.connect(input, resource(p, "input"));
for (var i=1; i < links; i++) {
  var p1 = split();
  connect(p, resource(p1, "input"));
  p = p1;
}
connect(p, output);

setAllActive();
