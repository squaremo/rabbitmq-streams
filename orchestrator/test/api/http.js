var http = Packages.com.meterware.httpunit;

var baseUrl = "http://localhost:8000";

function getResponse(path) {
  var c = new http.WebConversation();
  var req = new http.GetMethodWebRequest(baseUrl+path);
  var res = c.getResponse(req);
  return res;
}

function jsonGetResponse(path) {
  var res = getResponse(path);
  Test.areEqual("application/json", res.getContentType());
  Test.areEqual(200, res.getResponseCode());
  return res;
}

function verifyQueryResult(obj, expectedRows) {
  Test.isTrue(obj.hasOwnProperty('total'));
  Test.areEqual(expectedRows, obj['total']);
  Test.isTrue(obj.hasOwnProperty("values"));
  return obj['values'];
}
