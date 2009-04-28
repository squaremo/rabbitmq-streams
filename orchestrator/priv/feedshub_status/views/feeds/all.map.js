function(doc) {
    if (doc.type == "feed-status") emit(doc._id, doc.active);
}
