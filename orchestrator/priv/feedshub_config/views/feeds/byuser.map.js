function(doc) {
    if (doc.type == "feed") emit(doc.user, doc._id);
}
