function(keys, values, rereduce) {
  if (rereduce) {
    return values;
  } else {
    for (ind in keys) {
      var k = keys[ind];
      var output = {"terminals": []};
      for (t in values) {
        var v = values[t];
        if (v.type=="terminal") {
          output["terminals"].push(v);
        }
        else if (v.type=="server") {
          output["server"] = v;
        }
      }
      return output;
    }
  }
}
