function(doc) {
  if (doc.type == "terminal") {
    for (var ind=0; ind < doc.servers.length; i++) {
      emit(doc.servers[ind].server, doc);
    }
  }
  else if (doc.type=="server") {
    emit(doc._id, doc);
  }
}
