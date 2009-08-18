function setup() {
}

Test.requires("http.js"); // we'll stub this while compiling
Test.requires("json2.js");

function test_root_get() {
  var c = new http.WebConversation();
  var req = new http.GetMethodWebRequest("http://localhost:8000/");
  var res = c.getResponse(req);
  Test.areEqual("application/json", res.getContentType());
  Test.areEqual(200, res.getResponseCode());
  var body = res.getText();
  var bodyObj = JSON.parse(new String(body));
  Test.areEqual("RabbitMQ Streams", bodyObj['application']);
  Test.areEqual("prototype", bodyObj['version']);
}
