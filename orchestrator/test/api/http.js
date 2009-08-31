var http = Packages.com.meterware.httpunit;

var baseUrl = "http://localhost:8000";

function getResponse(path) {
  var c = new http.WebConversation();
  var req = new http.GetMethodWebRequest(baseUrl+path);
  if (getResponse.arguments.length > 1) {
    var headers = getResponse.arguments[1];
    for (k in headers) {
      req.setHeaderField(k, headers[k]);
    }
  }
  var res = c.getResponse(req);
  return res;
}

function jsonGetResponse(path) {
  var res = getResponse(path, {"Accept": "application/json"});
  Test.areEqual("application/json", res.getContentType());
  return res;
}

function postResponse(path, postdata, contenttype) {
  var c = new http.WebConversation();
  var is = new java.io.ByteArrayInputStream(new java.lang.String(postdata).getBytes());
  var req = new http.PostMethodWebRequest(baseUrl+path, is, contenttype);
  if (postResponse.arguments.length > 3) {
    var headers = postResponse.arguments[3];
    for (k in headers) {
      req.setHeaderField(k, headers[k]);
    }
  }
  var res = c.getResponse(req);
  return res;
}

function jsonPostResponse(path, obj) {
  var res = postResponse(path, JSON.stringify(obj), "application/json",
    {"accept": "application/json"});
  Test.areEqual("application/json", res.getContentType());
  return res;
}

function verifyQueryResult(obj, expectedRows) {
  Test.isTrue(obj.hasOwnProperty('total'));
  Test.areEqual(expectedRows, obj['total']);
  Test.isTrue(obj.hasOwnProperty("values"));
  return obj['values'];
}
