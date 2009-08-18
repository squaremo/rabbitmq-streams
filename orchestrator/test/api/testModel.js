function setup() {
  var f = feed("modeltest");
  setAllActive();
}

Test.requires("http.js");
Test.requires("json2.js");

function test_root_get() {
  var res = jsonGetResult('/');
  var body = res.getText();
  var bodyObj = JSON.parse(new String(body));
  Test.areEqual("RabbitMQ Streams", bodyObj['application']);
  Test.areEqual("prototype", bodyObj['version']);
}

function test_pipeline_index() {
}
