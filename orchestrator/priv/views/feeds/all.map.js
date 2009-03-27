function(doc) {
    if (doc.type == "feed") emit(doc._id, null);
}
