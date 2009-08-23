function setup() {
  var f = feed("processtest");
  setAllActive();
}

Test.requires("http.js");
Test.requires("json2.js");

function test_pipeline_process() {
  var res = jsonGetResponse('/process/pipeline/processtest');
  var json = JSON.parse(new String(res.getText()));
  Test.isTrue(json.hasOwnProperty('/process/pipeline/processtest'));
  Test.isTrue(json['/process/pipeline/test']);
}
