function(doc) {
    if (doc.type == "feed") emit(null, doc._id);
}
