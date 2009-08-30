function setup() {
  var f = feed("modeltest");
  setAllActive();
}

Test.requires("http.js");
Test.requires("json2.js");

function test_root_get() {
  var res = jsonGetResponse('/');
  var body = res.getText();
  var obj = JSON.parse(new String(body));
  Test.areEqual("RabbitMQ Streams", obj['application']);
  Test.areEqual("prototype", obj['version']);
}


function test_pipeline_index() {
  var res = jsonGetResponse('/model/pipeline/');
  var obj = JSON.parse(new String(res.getText()));
  var rows = verifyQueryResult(obj, 1);
  row = rows[0];
  Test.isTrue(row.hasOwnProperty('url'));
  Test.areEqual('/model/pipeline/modeltest', row['url']);
}
