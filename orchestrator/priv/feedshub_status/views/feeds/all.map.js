function(doc) {
  if (doc.type == "feed-status") emit(doc._id.replace(/_status/, ""), doc.active);
}
