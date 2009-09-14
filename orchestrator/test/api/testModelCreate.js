function setup() {
}

Test.requires("http.js");
Test.requires("json2.js");

var feedname = 'new';

function test_pipeline_put_new() {
  var path = '/model/pipeline/'+feedname;
  var res = putResponse(
    path, JSON.stringify({"type": "feed"}),
    "application/json", {"Accept": "application/json"});
  Test.areEqual(201, res.getResponseCode());
  Test.areEqual(path, res.getHeaderField("Location"));
  var res2 = jsonGetResponse(path);
  Test.areEqual(200, res2.getResponseCode());
}

// If-Match: * is used to say "I don't want to create a new one"

function test_pipeline_put_new_if_match_star() {
  var path = '/model/pipeline/'+feedname+'star';
  try {
    var res = putResponse(
      path, JSON.stringify({"type": "feed"}),
      "application/json", {"Accept": "application/json",
                           "If-Match": "*"});
    Test.fail("Should response with 412");
  }
  catch (e) {
    Test.areEqual(412, e.javaException.getResponseCode());
  }
}

// Create by posting to /model/pipeline

function test_pipeline_create_by_post() {
  var path = '/model/pipeline/';
  var res = postResponse(
    path, JSON.stringify({"type": "feed"}),
    "application/json", {"Accept": "application/json"});
  Test.areEqual(201, res.getResponseCode());
  var loc = res.getHeaderField("Location");
  var res2 = jsonGetResponse(loc);
  Test.areEqual(200, res2.getResponseCode());
}
