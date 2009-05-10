function(doc) {
  if (doc.type == "terminal-status") emit(doc._id.replace(/_status/, ""), doc.active);
}
