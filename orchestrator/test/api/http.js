var http = Packages.com.meterware.httpunit;

function jsonGetResult(path) {
  var c = new http.WebConversation();
  var req = new http.GetMethodWebRequest("http://localhost:8000"+path);
  var res = c.getResponse(req);
  Test.areEqual("application/json", res.getContentType());
  Test.areEqual(200, res.getResponseCode());
  return res;
}
