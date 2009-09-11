var feedname = 'modeltest';

function setup() {
  var f = feed(feedname);
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
  Test.areEqual('/model/pipeline/'+feedname, row['url']);
}

function test_pipeline_get() {
  var res = jsonGetResponse('/model/pipeline/' + feedname);
}

function test_pipeline_put_no_etag() {
  try {
    var res = putResponse('/model/pipeline/'+feedname,
      JSON.stringify({"type": "feed"}),
      "application/json", {"Accept": "application/json"});
    Test.fail("Should have reported a 409");
  }
  catch (e) {
    Test.areEqual(409, e.javaException.getResponseCode());
  }
}


function put_with_etag(etag) {
  return putResponse(
    '/model/pipeline/'+feedname,
    JSON.stringify({"type": "feed"}),
    "application/json", {"Accept": "application/json", "If-Match": etag});
}

function test_pipeline_put_wrong_etag() {
  try {
    put_with_etag("foobar");
    Test.fail("Should have reported a 412");
  }
  catch (e) {
    Test.areEqual(412, e.javaException.getResponseCode());
  }
}

function test_pipeline_put_multiple_wrong_etag() {
  try {
    put_with_etag("foobar, barfoo");
    Test.fail("Should have reported a 412");
  }
  catch (e) {
    Test.areEqual(412, e.javaException.getResponseCode());
  }
}

function test_pipeline_put_multiple_incl_right_etag() {
  var res = jsonGetResponse('/model/pipeline/'+feedname);
  var etag = res.getHeaderField("Etag");
  try {
    put_with_etag(etag + ", foobar");
    Test.fail("Should have reported a 412");
  }
  catch (e) {
    Test.areEqual(412, e.javaException.getResponseCode());
  }
}

function test_pipeline_put_star_etag() {
  try {
    put_with_etag("*");
    Test.fail("Should have reported a 409");
  }
  catch (e) {
    Test.areEqual(409, e.javaException.getResponseCode());
  }
}

function test_pipeline_put_right_etag() {
  var res = jsonGetResponse('/model/pipeline/'+feedname);
  var etag = res.getHeaderField("Etag");
  var res = put_with_etag(etag);
  Test.areEqual(204, res.getResponseCode());
}
