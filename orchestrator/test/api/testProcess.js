var feedname = "testfeed";

function setup() {
  var f = feed(feedname);
}

Test.requires("json2.js");
Test.requires("http.js");

function test_pipeline_state() {
  var url = '/process/pipeline/' + feedname;
  var res = jsonGetResponse(url);
  var body = res.getText();
  var obj = JSON.parse(new String(body));
  Test.isTrue(obj.hasOwnProperty('/process/pipeline/'+feedname));
}

function test_pipeline_unknown() {
  var url = '/process/pipeline/notknown';
  try {
    var res = getResponse(url);
    Test.fail("Should have reported an 404");
  }
  catch (e) {
    Test.areEqual(404, e.javaException.getResponseCode());
  }
}
