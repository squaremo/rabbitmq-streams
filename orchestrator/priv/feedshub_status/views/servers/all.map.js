function(doc) {
    if (doc.type == "server") emit(null, doc._id);
}
