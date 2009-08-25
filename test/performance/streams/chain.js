// This produces a chain of no-op JavaScript plugins arranged in a line.
//
// The purpose is to see how Streams performs when
// 1. running one JVM per plugin
// 2. running one JVM per feed

var links = 10;

var f = feed("chain");

var input = f.terminal(httppost_a);
var output = f.terminal(netprint12345);

var log = f.plugin({"type": "logger", "configuration": {}});

f.connect(input, resource(log, "input"));

function js() {
  return f.plugin({"type": "javascript",
                   "configuration":
                   {"function": "function(msg) {return msg;}"}});
}

var p = js();
f.connect(input, resource(p, "input"));
for (var i=1; i < links; i++) {
  var p1 = js();
  f.connect(resource(p, "output"), resource(p1, "input"));
  p = p1;
}
f.connect(resource(p, "output"), output);

setAllActive();
