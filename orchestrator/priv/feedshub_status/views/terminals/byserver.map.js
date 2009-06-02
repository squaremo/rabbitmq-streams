function(doc) {
  if (doc.type=="terminal") {
    for (var i=0; i < doc.servers.length; i++) {
      var output = {};
      for (k in doc) if (k != "servers") output[k] = doc[k];
      output["server"] = doc.servers[i];
      emit(doc.servers[i].server, output);
    }
  }
  else if (doc.type=="server") {
    emit(doc._id, doc);
  }
}
